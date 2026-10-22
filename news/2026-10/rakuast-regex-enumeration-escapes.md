RakuAST regex conversion now keeps backspace and literal letter escapes in
character-class enumerations, and ignores embedded comments between class
arithmetic terms. `roast/S05-metasyntax/regex.t` and
`roast/S05-metasyntax/charset.t` now pass under the RakuAST frontend.
