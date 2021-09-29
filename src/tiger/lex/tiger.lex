%filenames = "scanner"

 /*
  * Please don't modify the lines above.
  */

 /* You can add lex definitions here. */
 /* TODO: Put your lab2 code here */

%x COMMENT STR IGNORE

%%

 /*
  * Below is examples, which you can wipe out
  * and write regular expressions and actions of your own.
  *
  * All the tokens:
  *   Parser::ID
  *   Parser::STRING
  *   Parser::INT
  *   Parser::COMMA
  *   Parser::COLON
  *   Parser::SEMICOLON
  *   Parser::LPAREN
  *   Parser::RPAREN
  *   Parser::LBRACK
  *   Parser::RBRACK
  *   Parser::LBRACE
  *   Parser::RBRACE
  *   Parser::DOT
  *   Parser::PLUS
  *   Parser::MINUS
  *   Parser::TIMES
  *   Parser::DIVIDE
  *   Parser::EQ
  *   Parser::NEQ
  *   Parser::LT
  *   Parser::LE
  *   Parser::GT
  *   Parser::GE
  *   Parser::AND
  *   Parser::OR
  *   Parser::ASSIGN
  *   Parser::ARRAY
  *   Parser::IF
  *   Parser::THEN
  *   Parser::ELSE
  *   Parser::WHILE
  *   Parser::FOR
  *   Parser::TO
  *   Parser::DO
  *   Parser::LET
  *   Parser::IN
  *   Parser::END
  *   Parser::OF
  *   Parser::BREAK
  *   Parser::NIL
  *   Parser::FUNCTION
  *   Parser::VAR
  *   Parser::TYPE
  */

 /* handle comments */
"/*" {
       adjust();
       begin(StartCondition__::COMMENT);
       ++comment_level_;
     }
<COMMENT> {
  "/*" {
         adjust();
         ++comment_level_;
       }
  "*/" {
        adjust();
        --comment_level_;
        if (comment_level_ == 1) {
          begin(StartCondition__::INITIAL);
        }
       }
  .|\n {adjust();}
  <<EOF>> {adjust(); errormsg_->Error(errormsg_->tok_pos_, "unclosed comments");}
}

 /* reserved words */
"array" {adjust(); return Parser::ARRAY;}
"do" {adjust(); return Parser::DO;}
"else" {adjust(); return Parser::ELSE;}
"end" {adjust(); return Parser::END;}
"for" {adjust(); return Parser::FOR;}
"function" {adjust(); return Parser::FUNCTION;}
"if" {adjust(); return Parser::IF;}
"in" {adjust(); return Parser::IN;}
"let" {adjust(); return Parser::LET;}
"nil" {adjust(); return Parser::NIL;}
"of" {adjust(); return Parser::OF;}
"then" {adjust(); return Parser::THEN;}
"to" {adjust(); return Parser::TO;}
"type" {adjust(); return Parser::TYPE;}
"var" {adjust(); return Parser::VAR;}
"while" {adjust(); return Parser::WHILE;}

 /* operators */
"+" {adjust(); return Parser::PLUS;}
"-" {adjust(); return Parser::MINUS;}
"*" {adjust(); return Parser::TIMES;}
"/" {adjust(); return Parser::DIVIDE;}
"&" {adjust(); return Parser::AND;}
"|" {adjust(); return Parser::OR;}

"<" {adjust(); return Parser::LT;}
"<=" {adjust(); return Parser::LE;}
"=" {adjust(); return Parser::EQ;}
">" {adjust(); return Parser::GT;}
">=" {adjust(); return Parser::GE;}
"<>" {adjust(); return Parser::NEQ;}

":=" {adjust(); return Parser::ASSIGN;}

 /* punctucations */
"." {adjust(); return Parser::DOT;}
"," {adjust(); return Parser::COMMA;}
":" {adjust(); return Parser::COLON;}
";" {adjust(); return Parser::SEMICOLON;}
"(" {adjust(); return Parser::LPAREN;}
")" {adjust(); return Parser::RPAREN;}
"[" {adjust(); return Parser::LBRACK;}
"]" {adjust(); return Parser::RBRACK;}
"{" {adjust(); return Parser::LBRACE;}
"}" {adjust(); return Parser::RBRACE;}

 /* identifiers */
[[:alpha:]][[:alnum:]_]* {adjust(); return Parser::ID;}

 /* literals */
[[:digit:]]+ {adjust(); return Parser::INT;}

 /* strings */
\" {adjust(); begin(StartCondition__::STR);}
<STR> {
  \" {
      adjustStr();
      begin(StartCondition__::INITIAL);
      setMatched(unescape(matched().substr(0, length() - 1)));
      return Parser::STRING;
     }
  \\\"|.|\n {more();}
  <<EOF>> {adjust(); errormsg_->Error(errormsg_->tok_pos_, "unclosed string literal");}
}

 /*
  * skip white space chars.
  * space, tabs and LF
  */
[ \t]+ {adjust();}
\n {adjust(); errormsg_->Newline();}

 /* illegal input */
. {adjust(); errormsg_->Error(errormsg_->tok_pos_, "illegal token");}
