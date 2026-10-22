# A lexical Proxy subclass shows its visible name in `.^name`

`my class T is Proxy {}; T.new(...).VAR.^name` answered the class's mangled storage name
(`T\01`, NUL byte included). The storage name stays the class's identity, but `.^name` now
reports the visible name, as it does for an ordinary instance of a lexical class (#14261).
