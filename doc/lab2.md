# Lab 2

## Comments Handling

The basic idea is to use start conditions to determine whether we are inside a comment or not, and use `comment_level_` to handle nested comments.

When encountering a start of comments (`/*`) in any state, increase `comment_level_`; if the current start state is `INITIAL`, change it to `COMMENT`.

When encountering an end of comments (`*/`) in `COMMENT` state, decrease `comment_level_`; if `comment_level_` is 1, change start state back to `INITIAL`.

When in `COMMENT` state, ignore any character sequence except `/*` and `*/`, including line breaks.

## Strings Handling

Similar to comments, we use start conditions to determine whether we are inside a string or not. Besides, `string_buf_` is used to store unescaped string, this enables us to utilize the power of regular expressions to handle escaped characters in parser.

For each supported escape sequence in reference manual, a regular expression is written for it, in 'action' code block, we convert it to the unescaped character with custom logic, and append it to `string_buf_`.

For multi-line string support, the `IGNORE` start condition is used for better error handling and error message (discussed later).

## Error Handling

If the ASCII number in escape sequence `\ddd` is out of range, an error will be thrown, indicating invalid escape sequence. (see [ASCII](https://en.wikipedia.org/wiki/ASCII))

If the control character in escape sequence `\^c` is invalid, an error will be thrown, indicating invalid escape sequence. (see [C0 and C1 control codes](https://en.wikipedia.org/wiki/C0_and_C1_control_codes))

Other escape sequences like `\c` where `c` is not `n` or `t` will also incur an error, indicating invalid escape sequence.

## End-Of-File Handling

If the parser reaches the end of file in `COMMENT` state, an error will be thrown, indicating unclosed comments.

Also, if the parser reaches the end of file in `STR` or `IGNORE` state, an error will be thrown, indicating an unclosed string.

## Other Interesting Features

- If line breaks are detected in strings, an error will be thrown.

- If invalid characters are detected in `\f___f\` sequence, an error will be thrown.

- Special care for line breaks in multiline strings and comments to generate corrent line number in error message.
