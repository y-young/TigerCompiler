#ifndef TIGER_LEX_SCANNER_H_
#define TIGER_LEX_SCANNER_H_

#include <algorithm>
#include <cstdarg>
#include <iostream>
#include <string>

#include "scannerbase.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/parse/parserbase.h"

class Scanner : public ScannerBase {
public:
  Scanner() = delete;
  explicit Scanner(std::string_view fname, std::ostream &out = std::cout)
      : ScannerBase(std::cin, out), comment_level_(1), char_pos_(1),
        errormsg_(std::make_unique<err::ErrorMsg>(fname)) {
    switchStreams(errormsg_->infile_, out);
  }

  /**
   * Output an error
   * @param message error message
   */
  void Error(int pos, std::string message, ...) {
    va_list ap;
    va_start(ap, message);
    errormsg_->Error(pos, message, ap);
    va_end(ap);
  }

  /**
   * Getter for `tok_pos_`
   */
  [[nodiscard]] int GetTokPos() const { return errormsg_->GetTokPos(); }

  /**
   * Transfer the ownership of `errormsg_` to the outer scope
   * @return unique pointer to errormsg
   */
  [[nodiscard]] std::unique_ptr<err::ErrorMsg> TransferErrormsg() {
    return std::move(errormsg_);
  }

  int lex();

private:
  int comment_level_;
  std::string string_buf_;
  int char_pos_;
  std::unique_ptr<err::ErrorMsg> errormsg_;

  /**
   * NOTE: do not change all the funtion signature below, which is used by
   * flexc++ internally
   */
  int lex__();
  int executeAction__(size_t ruleNr);

  void print();
  void preCode();
  void postCode(PostEnum__ type);
  void adjust();
  void adjustStr();

  std::string unescape(const std::string& str);
};

inline int Scanner::lex() { return lex__(); }

inline void Scanner::preCode() {
  // Optionally replace by your own code
}

inline void Scanner::postCode(PostEnum__ type) {
  // Optionally replace by your own code
}

inline void Scanner::print() { print__(); }

inline void Scanner::adjust() {
  errormsg_->tok_pos_ = char_pos_;
  char_pos_ += length();
}

inline void Scanner::adjustStr() { char_pos_ += length(); }

inline std::string Scanner::unescape(const std::string& str) {
  int len = str.size();
  int pos = 0;
  std::string result;
  while (pos < len) {
    char current = str[pos];
    char next = pos < len - 1 ? str[pos + 1] : '\0';
    if (current == '\\') {
      switch (next) {
      case '\0':
        errormsg_->Error(errormsg_->tok_pos_, "illegal escape sequence");
        ++pos;
        break;
      case 'n':
        result += '\n';
        pos += 2;
        break;
      case 't':
        result += '\t';
        pos += 2;
        break;
      case '\\':
        result += '\\';
        pos += 2;
        break;
      case '"':
        result += '"';
        pos += 2;
        break;
      case '^': { // \^c
        char c = pos + 1 < len - 1 ? str[pos + 2] : '\0';
        if (c >= '@' && c <= '_') {
          result += char(c - '@');
          pos += 3;
        } else {
          errormsg_->Error(errormsg_->tok_pos_, "illegal escape sequence");
          ++pos;
        }
        break;
      }
      case '\n':
      case '\r':
      case '\t':
      case ' ':
        ++pos;
        while(pos < len && str[pos] != '\\') {
          ++pos;
        }
        ++pos;
        break;
      default: {
        if (next >= '0' && next <= '9') { // \ddd
          if (pos >= len - 3) {
            errormsg_->Error(errormsg_->tok_pos_, "illegal escape sequence");
            ++pos;
            break;
          }
          std::string ddd = str.substr(pos + 1, pos + 4);
          int ascii = std::stoi(ddd);
          if (ascii > 127) {
            errormsg_->Error(errormsg_->tok_pos_, "illegal escape sequence");
            ++pos;
          } else {
            char c = ascii;
            result += c;
            pos += 4;
          }
        } else {
          errormsg_->Error(errormsg_->tok_pos_, "illegal escape sequence");
          ++pos;
        }
        break;
      }
      }
    } else {
      result += current;
      ++pos;
    }
  }
  return result;
}

#endif // TIGER_LEX_SCANNER_H_
