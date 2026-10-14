#include "../simdjson/singleheader/simdjson.h"
using namespace simdjson;
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

  // Parses a number token as coefficient * 10^exponent. Writes to out:
  //   [0] coefficient, or its sign when the digits do not fit in int64
  //   [1] exponent
  //   [2..5] integer digits and fraction digits as (pointer, length) pairs
  // Returns 0, -1 when the coefficient must be built from the digits,
  // or an error code.
  int get_scientific(ondemand::value &val, int64_t *out) {
    std::string_view tok = val.raw_json_token();
    const char *p = tok.data();
    const char *end = p + tok.size();
    auto digit = [&] { return p < end && unsigned(*p - '0') < 10; };
    if (p == end || !(*p == '-' || unsigned(*p - '0') < 10)) { return INCORRECT_TYPE; }
    bool neg = *p == '-';
    if (neg) { ++p; }
    uint64_t m = 0;
    const char *intStart = p;
    while (digit()) { m = m * 10 + unsigned(*p - '0'); ++p; }
    const char *intEnd = p;
    if (intStart == intEnd || (*intStart == '0' && intEnd - intStart > 1)) { return NUMBER_ERROR; }
    const char *fracStart = p;
    const char *fracEnd = p;
    if (p < end && *p == '.') {
      fracStart = ++p;
      while (digit()) { m = m * 10 + unsigned(*p - '0'); ++p; }
      fracEnd = p;
      if (fracStart == fracEnd) { return NUMBER_ERROR; }
    }
    int64_t e = 0;
    if (p < end && (*p == 'e' || *p == 'E')) {
      ++p;
      bool eneg = p < end && *p == '-';
      if (p < end && (*p == '+' || *p == '-')) { ++p; }
      const char *expStart = p;
      while (p < end && *p == '0') { ++p; }
      const char *expDigits = p;
      while (digit()) {
        if (p - expDigits == 18) { return NUMBER_ERROR; }
        e = e * 10 + (*p - '0');
        ++p;
      }
      if (p == expStart) { return NUMBER_ERROR; }
      if (eneg) { e = -e; }
    }
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) { ++p; }
    if (p != end) { return NUMBER_ERROR; }
    out[1] = e - (fracEnd - fracStart);
    if ((intEnd - intStart) + (fracEnd - fracStart) <= 18) {
      out[0] = neg ? -int64_t(m) : int64_t(m);
      return 0;
    }
    out[0] = neg ? -1 : 1;
    out[2] = int64_t(reinterpret_cast<intptr_t>(intStart));
    out[3] = intEnd - intStart;
    out[4] = int64_t(reinterpret_cast<intptr_t>(fracStart));
    out[5] = fracEnd - fracStart;
    return -1;
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
