#include "../simdjson/singleheader/simdjson.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <cstdint>
using namespace simdjson;

// Serialize an on-demand value subtree into a contiguous tape buffer
// in one C++ call, so Haskell can build an Aeson Value without
// per-node FFI roundtrips.
//
// Tape entry format (little-endian, unaligned):
//   0x00 null         : tag(1)
//   0x01 true         : tag(1)
//   0x02 false        : tag(1)
//   0x03 int          : tag(1) + i64(8)
//   0x05 raw_number   : tag(1) + u32 len(4) + ptr(8)   // for Scientific
//   0x06 string       : tag(1) + u32 len(4) + ptr(8)
//   0x07 array        : tag(1) + u32 count(4) + N children
//   0x08 object       : tag(1) + u32 count(4) + N x (u32 keylen(4) + ptr(8) + child)
//
// String/key pointers reference the parser's internal buffers and stay
// valid for the parser+document lifetime, which the Haskell decoder
// keeps alive across the whole decode. simdjson's per-parser string
// buffer is pre-sized at parser construction and only ever appended
// to within a document, so earlier string_view pointers do not get
// invalidated by later get_string / unescaped_key calls.
//
// Platform assumption: the Haskell reader uses peekByteOff at the same
// offsets the writer emits, so both sides must agree on byte order and
// must tolerate unaligned scalar loads. Verified on x86_64 and
// aarch64-darwin. Strict-alignment ARM targets (rare in practice for
// Haskell) would need padding inserted between fields.

namespace {
struct Tape {
  uint8_t *data;
  size_t   len;
  size_t   cap;
};

inline bool tape_reserve(Tape &t, size_t extra) {
  // Guard against size_t overflow on pathological inputs. Both the
  // need calculation and the doubling can wrap to 0 and produce an
  // infinite loop or a too-small allocation otherwise.
  size_t need = t.len + extra;
  if (need < t.len) return false;
  if (need <= t.cap) return true;
  size_t newcap = t.cap ? t.cap : 4096;
  while (newcap < need) {
    if (newcap > (SIZE_MAX >> 1)) return false;
    newcap <<= 1;
  }
  uint8_t *p = (uint8_t*)std::realloc(t.data, newcap);
  if (!p) return false;
  t.data = p;
  t.cap = newcap;
  return true;
}

// Precondition: caller has already reserved at least one byte.
inline void emit_u8(Tape &t, uint8_t v) {
  assert(t.len < t.cap);
  t.data[t.len++] = v;
}

inline void emit_u32_at(Tape &t, size_t off, uint32_t v) {
  std::memcpy(t.data + off, &v, sizeof(v));
}

inline void emit_u32(Tape &t, uint32_t v) {
  std::memcpy(t.data + t.len, &v, sizeof(v));
  t.len += sizeof(v);
}

inline void emit_i64(Tape &t, int64_t v) {
  std::memcpy(t.data + t.len, &v, sizeof(v));
  t.len += sizeof(v);
}

inline void emit_ptr(Tape &t, const char *p) {
  std::memcpy(t.data + t.len, &p, sizeof(p));
  t.len += sizeof(p);
}

inline void emit_strref(Tape &t, const char *data, size_t len) {
  // u32 len + ptr
  emit_u32(t, (uint32_t)len);
  emit_ptr(t, data);
}

// walk_value recurses on JSON nesting depth. Bounded by simdjson's
// parse-time depth limit (default 1024), so worst-case stack usage is
// modest. We do not add an explicit guard here.
static error_code walk_value(ondemand::value v, Tape &t);

static error_code walk_array(ondemand::array arr, Tape &t) {
  // tag + length placeholder
  if (!tape_reserve(t, 1 + 4)) return MEMALLOC;
  emit_u8(t, 0x07);
  // len_off is an offset (not a pointer) so it stays valid across any
  // tape_reserve realloc that happens inside the loop body.
  size_t len_off = t.len;
  emit_u32(t, 0);
  uint32_t count = 0;
  for (auto child : arr) {
    ondemand::value cv;
    if (auto e = child.get(cv)) return e;
    if (auto e = walk_value(cv, t)) return e;
    ++count;
  }
  emit_u32_at(t, len_off, count);
  return SUCCESS;
}

static error_code walk_object(ondemand::object obj, Tape &t) {
  if (!tape_reserve(t, 1 + 4)) return MEMALLOC;
  emit_u8(t, 0x08);
  // len_off is an offset (not a pointer) so it stays valid across any
  // tape_reserve realloc that happens inside the loop body.
  size_t len_off = t.len;
  emit_u32(t, 0);
  uint32_t count = 0;
  for (auto field : obj) {
    std::string_view k;
    if (auto e = field.unescaped_key(true).get(k)) return e;
    if (!tape_reserve(t, 4 + sizeof(const char*))) return MEMALLOC;
    emit_strref(t, k.data(), k.size());
    // field.value() returns simdjson_result<value>. Extract explicitly
    // rather than relying on implicit-conversion-to-value, which does
    // not check the result's error.
    ondemand::value cv;
    if (auto e = field.value().get(cv)) return e;
    if (auto e = walk_value(cv, t)) return e;
    ++count;
  }
  emit_u32_at(t, len_off, count);
  return SUCCESS;
}

static error_code walk_value(ondemand::value v, Tape &t) {
  ondemand::json_type ty;
  if (auto e = v.type().get(ty)) return e;
  switch (ty) {
    case ondemand::json_type::null: {
      if (!tape_reserve(t, 1)) return MEMALLOC;
      // Consume the value: type() alone doesn't advance the iterator.
      bool is_n;
      if (auto e = v.is_null().get(is_n)) return e;
      emit_u8(t, 0x00);
      return SUCCESS;
    }
    case ondemand::json_type::boolean: {
      bool b;
      if (auto e = v.get_bool().get(b)) return e;
      if (!tape_reserve(t, 1)) return MEMALLOC;
      emit_u8(t, b ? 0x01 : 0x02);
      return SUCCESS;
    }
    case ondemand::json_type::number: {
      ondemand::number_type nt;
      if (auto e = v.get_number_type().get(nt)) return e;
      if (nt == ondemand::number_type::signed_integer) {
        int64_t i;
        if (auto e = v.get_int64().get(i)) return e;
        if (!tape_reserve(t, 1 + 8)) return MEMALLOC;
        emit_u8(t, 0x03);
        emit_i64(t, i);
        return SUCCESS;
      }
      // Non-int (unsigned, floating-point, big-int): emit raw token,
      // let Haskell parse it as Scientific.
      std::string_view sv = v.raw_json_token();
      if (!tape_reserve(t, 1 + 4 + sizeof(const char*))) return MEMALLOC;
      emit_u8(t, 0x05);
      emit_strref(t, sv.data(), sv.size());
      return SUCCESS;
    }
    case ondemand::json_type::string: {
      // get_string(true) enables U+FFFD replacement of invalid UTF-8,
      // matching unescaped_key(true) in walk_object and T.decodeUtf8Lenient
      // on the Haskell side. Without this we would accept invalid UTF-8
      // in object keys but reject it in string values.
      std::string_view sv;
      if (auto e = v.get_string(true).get(sv)) return e;
      if (!tape_reserve(t, 1 + 4 + sizeof(const char*))) return MEMALLOC;
      emit_u8(t, 0x06);
      emit_strref(t, sv.data(), sv.size());
      return SUCCESS;
    }
    case ondemand::json_type::array: {
      ondemand::array arr;
      if (auto e = v.get_array().get(arr)) return e;
      return walk_array(arr, t);
    }
    case ondemand::json_type::object: {
      ondemand::object obj;
      if (auto e = v.get_object().get(obj)) return e;
      return walk_object(obj, t);
    }
    default:
      return INCORRECT_TYPE;
  }
}
} // namespace

extern "C" {
  // out_err_loc: on error, points into the simdjson document buffer at
  // the position where the walker gave up. Up to SIMDJSON_PADDING bytes
  // past this address are safely readable thanks to padded_string.
  // Set to nullptr on success.
  //
  // All three out params are written before any early return, so the
  // Haskell caller can safely peek them without first initializing.
  error_code build_aeson_tape(
      ondemand::value &v,
      uint8_t **out_data,
      size_t *out_len,
      const char **out_err_loc) {
    *out_data = nullptr;
    *out_len = 0;
    *out_err_loc = nullptr;

    Tape t{};
    if (!tape_reserve(t, 4096)) return MEMALLOC;

    auto e = walk_value(v, t);
    *out_data = t.data;
    *out_len  = t.len;
    if (e != SUCCESS) {
      const char *loc = nullptr;
      (void)v.current_location().get(loc);
      *out_err_loc = loc;
    }
    return e;
  }

  void free_aeson_tape(uint8_t *p) {
    std::free(p);
  }

  // Like build_aeson_tape but operates at the document level, so it
  // handles top-level scalar documents (number, string, boolean, null)
  // that fail with SCALAR_DOCUMENT_AS_VALUE when accessed via
  // doc.get_value(). For containers, dispatches to walk_value via
  // doc.get_array() / doc.get_object().
  error_code build_aeson_tape_doc(
      ondemand::parser &parser,
      padded_string &input,
      ondemand::document &doc,
      uint8_t **out_data,
      size_t *out_len,
      const char **out_err_loc) {
    *out_data = nullptr;
    *out_len = 0;
    *out_err_loc = nullptr;

    if (auto e = parser.iterate(input).get(doc)) return e;

    Tape t{};
    if (!tape_reserve(t, 4096)) return MEMALLOC;

    ondemand::json_type ty;
    error_code err = doc.type().get(ty);
    if (err == SUCCESS) {
      switch (ty) {
        case ondemand::json_type::array: {
          ondemand::array arr;
          if ((err = doc.get_array().get(arr))) break;
          err = walk_array(arr, t);
          break;
        }
        case ondemand::json_type::object: {
          ondemand::object obj;
          if ((err = doc.get_object().get(obj))) break;
          err = walk_object(obj, t);
          break;
        }
        case ondemand::json_type::null: {
          bool is_n;
          if ((err = doc.is_null().get(is_n))) break;
          if (!tape_reserve(t, 1)) { err = MEMALLOC; break; }
          emit_u8(t, 0x00);
          break;
        }
        case ondemand::json_type::boolean: {
          bool b;
          if ((err = doc.get_bool().get(b))) break;
          if (!tape_reserve(t, 1)) { err = MEMALLOC; break; }
          emit_u8(t, b ? 0x01 : 0x02);
          break;
        }
        case ondemand::json_type::number: {
          ondemand::number_type nt;
          if ((err = doc.get_number_type().get(nt))) break;
          if (nt == ondemand::number_type::signed_integer) {
            int64_t i;
            if ((err = doc.get_int64().get(i))) break;
            if (!tape_reserve(t, 1 + 8)) { err = MEMALLOC; break; }
            emit_u8(t, 0x03);
            emit_i64(t, i);
          } else {
            std::string_view sv;
            if ((err = doc.raw_json_token().get(sv))) break;
            if (!tape_reserve(t, 1 + 4 + sizeof(const char*))) { err = MEMALLOC; break; }
            emit_u8(t, 0x05);
            emit_strref(t, sv.data(), sv.size());
          }
          break;
        }
        case ondemand::json_type::string: {
          // Lenient UTF-8 (replacement on invalid bytes), see walk_value.
          std::string_view sv;
          if ((err = doc.get_string(true).get(sv))) break;
          if (!tape_reserve(t, 1 + 4 + sizeof(const char*))) { err = MEMALLOC; break; }
          emit_u8(t, 0x06);
          emit_strref(t, sv.data(), sv.size());
          break;
        }
        default:
          err = INCORRECT_TYPE;
      }
    }

    *out_data = t.data;
    *out_len = t.len;
    if (err != SUCCESS) {
      const char *loc = nullptr;
      (void)doc.current_location().get(loc);
      *out_err_loc = loc;
    }
    return err;
  }
}

extern "C" {
  ondemand::parser *parser_init(size_t max_cap) {
    return new ondemand::parser{max_cap};
  }

  void parser_destroy(ondemand::parser *parser) {
    delete parser;
  }

  ondemand::document *make_document() {
    return new ondemand::document{};
  }

  void delete_document(ondemand::document *doc) {
    delete doc;
  }

  padded_string *make_input(const char *bytes, size_t len) {
    return new padded_string{bytes, len};
  }

  void delete_input(padded_string *str) {
    delete str;
  }

  error_code get_document_value(
      ondemand::parser &parser,
      padded_string &input,
      ondemand::document &doc,
      ondemand::value &out) {
    auto error = parser.iterate(input).get(doc);
    if (error != SUCCESS) { return error; }
    return doc.get_value().get(out);
  }

  error_code at_pointer(
      const char *pointer,
      size_t len,
      ondemand::document &doc,
      ondemand::value &out) {
    std::string_view pointerSv { pointer, len };
    return doc.at_pointer(pointerSv).get(out);
  }

  error_code get_object_from_value(ondemand::value &val) {
    return val.get_object().error();
  }

  error_code get_object_iter_from_value(ondemand::value &val) {
    ondemand::object obj;
    auto error = val.get_object().get(obj);
    if (error != SUCCESS) { return error; }
    return obj.begin().error();
  }

  bool obj_iter_is_done(ondemand::object_iterator &obj) {
    return obj.operator==(obj);
  }

  error_code obj_iter_get_current(
      ondemand::object_iterator &obj,
      const char **key,
      size_t *len,
      ondemand::value &out) {
    ondemand::field f;
    auto error = obj.operator*().get(f);
    if (error != SUCCESS) { return error; }
    std::string_view uek;
    error = f.unescaped_key(true).get(uek);
    *key = uek.data();
    *len = uek.length();
    out = f.value();
    return error;
  }

  void obj_iter_move_next(ondemand::object_iterator &obj) {
    ++obj;
  }

  error_code get_array_from_value(
      ondemand::value &val,
      ondemand::array &out) {
    return val.get_array().get(out);
  }

  error_code get_array_len_from_value(
      ondemand::value &val,
      size_t &len) {
    ondemand::array out;
    auto error = val.get_array().get(out);
    if (error) { return error; }
    return out.count_elements().get(len);
  }

  error_code int_array(ondemand::array &arr, int64_t out[]) {
    for(auto x : arr) {
      auto error = x.get_int64().get(*out);
      if (error) { return error; }
      ++out;
    }
    return SUCCESS;
  }

  error_code double_array(ondemand::array &arr, double out[]) {
    for(auto x : arr) {
      auto error = x.get_double().get(*out);
      if (error) { return error; }
      ++out;
    }
    return SUCCESS;
  }

  error_code get_array_iter_from_value(ondemand::value &val) {
    ondemand::array arr;
    auto error = val.get_array().get(arr);
    if (error != SUCCESS) { return error; }
    return arr.begin().error();
  }

  error_code get_array_iter_len_from_value(ondemand::value &val, size_t &len) {
    ondemand::array arr;
    auto error = val.get_array().get(arr);
    if (error != SUCCESS) { return error; }
    error = arr.begin().error();
    if (error != SUCCESS) { return error; }
    return arr.count_elements().get(len);
  }

  bool arr_iter_is_done(ondemand::array_iterator &arr) {
    return arr.operator==(arr);
  }

  error_code arr_iter_get_current(ondemand::array_iterator &arr, ondemand::value &out) {
    return arr.operator*().get(out);
  }

  void arr_iter_move_next(ondemand::array_iterator &arr) {
    ++arr;
  }

  void reset_array(ondemand::array &arr) {
    arr.reset();
  }

  void reset_object(ondemand::object &obj) {
    obj.reset();
  }

  error_code find_field(
      ondemand::object &obj,
      const char *key,
      size_t len,
      ondemand::value &out) {
    std::string_view keySv { key, len };
    return obj.find_field(keySv).get(out);
  }

  error_code find_field_unordered(
      ondemand::object &obj,
      const char *key,
      size_t len,
      ondemand::value &out) {
    std::string_view keySv { key, len };
    return obj.find_field_unordered(keySv).get(out);
  }

  error_code get_int(ondemand::value &val, int64_t &out) {
    return val.get_int64().get(out);
  }

  error_code get_uint(ondemand::value &val, uint64_t &out) {
    return val.get_uint64().get(out);
  }

  error_code get_double(ondemand::value &val, double &out) {
    return val.get_double().get(out);
  }

  error_code get_bool(ondemand::value &val, bool &out) {
    return val.get_bool().get(out);
  }

  error_code get_string(
      ondemand::value &val,
      const char **out,
      size_t &len) {
    std::string_view buf;
    auto error = val.get_string().get(buf);
    *out = buf.data();
    len = buf.length();
    return error;
  }

  void get_raw_json_token(
      ondemand::value &val,
      const char **out,
      size_t &len) {
    std::string_view buf = val.raw_json_token();
    *out = buf.data();
    len = buf.length();
  }

  error_code get_raw_json(
      ondemand::value &val,
      const char **out,
      size_t &len) {
    std::string_view buf;
    auto error = val.raw_json().get(buf);
    if (error) {
      return error;
    }
    *out = buf.data();
    len = buf.length();
    return SUCCESS;
  }

  error_code is_null(ondemand::value &val, bool &out) {
    return val.is_null().get(out);
  }

  error_code get_type(ondemand::value &val, ondemand::json_type &out) {
    return val.type().get(out);
  }

  const char *get_error_message(error_code error) {
    return error_message(error);
  }

}
