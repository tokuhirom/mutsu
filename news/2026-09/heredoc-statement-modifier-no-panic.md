# Heredoc statement modifiers no longer panic on multibyte text

The parser now avoids slicing a non-contiguous heredoc remainder by byte-length subtraction when deciding whether a `try` or `gather` statement ends with a block. Such heredocs now produce normal parse results instead of panicking inside a multibyte character.
