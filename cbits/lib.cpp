#include "./simdjson/simdjson.h"
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

  error_code get_iterator(
      ondemand::parser &parser, 
      padded_string &input, 
      ondemand::document &out) {
    return parser.iterate(input).get(out);
  }

  error_code get_document_value(
      ondemand::document &doc, 
      ondemand::value &out) {
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

  error_code get_object_from_value(
      ondemand::value &val, 
      ondemand::object &out) {
    return val.get_object().get(out);
  }

  error_code get_object_iter_from_value(
      ondemand::value &val, 
      ondemand::object_iterator &iterOut) {
    ondemand::object obj;
    auto error = val.get_object().get(obj);
    if (error != SUCCESS) { return error; }
    return obj.begin().get(iterOut);
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
    error = f.unescaped_key().get(uek);
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

  error_code get_array_iter_from_value(
      ondemand::value &val, 
      ondemand::array_iterator &iterOut) {
    ondemand::array arr;
    auto error = val.get_array().get(arr);
    if (error != SUCCESS) { return error; }
    return arr.begin().get(iterOut);
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

  bool is_null(ondemand::value &val) {
    return val.is_null();
  }

  error_code current_location(
      ondemand::document &doc, 
      const char **out) {
    return doc.current_location().get(*out);
  }

  void to_debug_string(ondemand::document &doc, char *out, size_t &len) {
    std::string str = doc.to_debug_string();
    strcpy(out, str.c_str());
    len = str.length();
  }

  const char *get_error_message(error_code error) {
    return error_message(error);
  }

}
