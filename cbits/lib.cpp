#include "./simdjson.h"
#include <iostream>
using namespace simdjson;
extern "C" {

  ondemand::parser *parser_init() {
    return new ondemand::parser();
  }

  void parser_destroy(ondemand::parser *parser) {
    parser->~parser();
  }

  void get_iterator(
      ondemand::parser *parser, 
      char *bytes, 
      size_t len, 
      ondemand::document &out, 
      error_code &error) {
    auto json = padded_string(bytes, len);
    parser->iterate(json).tie(out, error);
  }

  const char *get_error_message(error_code &error) {
    return error_message(error);
  }

  void get_raw_json_str(
      ondemand::document *docPtr, 
      std::string_view &out, 
      error_code &error) {
    docPtr->raw_json().tie(out, error);
  }

  void get_document_value(
      ondemand::document *docPtr, 
      ondemand::value &out, 
      error_code &error) {
    docPtr->get_value().tie(out, error);
  }

  void get_object_from_value(
      ondemand::value *valPtr, 
      ondemand::object &out, 
      error_code &error) {
    valPtr->get_object().tie(out, error);
  }

  void get_array_from_value(
      ondemand::value *valPtr, 
      ondemand::array &out, 
      int64_t *len, 
      error_code &error) {
    ondemand::array arrBuf;
    valPtr->get_array().tie(arrBuf, error);
    out = arrBuf;
    *len = arrBuf.count_elements();
  }

  void get_array_elems(ondemand::array &arrPtr, ondemand::value **out) {
    size_t index = 0;
    for (ondemand::value elem : arrPtr) { 
      *out[index++] = elem;
    }
  }

  void find_field_unordered(
      ondemand::object *objPtr, 
      char *key, 
      ondemand::value &out, 
      error_code &error) {
    objPtr->find_field_unordered(key).tie(out, error);
  }

  void get_int(ondemand::value *valPtr, int64_t &out, error_code &error) {
    valPtr->get_int64().tie(out, error);
  }

  void get_string(ondemand::value *valPtr, const char **out, error_code &error) {
    std::string_view buf;
    valPtr->get_string().tie(buf, error);
    *out = std::string{buf}.c_str();
  }

  ondemand::value **make_values_array(size_t len, ondemand::value **out) {
    for (size_t i = 0; i < len; ++i) {
      out[i] = new ondemand::value();
    }
    return out;
  }

  void delete_values_array(size_t *len, ondemand::value **vals) {
    for (size_t i = 0; i < *len; ++i) {
      delete vals[i];
    }
  }

}
