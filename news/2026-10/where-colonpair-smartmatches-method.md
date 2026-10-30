# A bare colonpair `where` clause smartmatches by calling the method the key names

`multi method filetype(:$decode where :!so)` / `sub f($x where :so)`: the smartmatch the
parameter binder used for a non-block `where` lacked the `Any ~~ Pair` arm (call the method
the key names on the topic, compare its truthiness with the pair's value), which only the
VM's smartmatch had, so such a parameter rejected every argument and the call died with
"Cannot resolve caller". The arm now lives in `Interpreter::smart_match_inner`. Archive::Libarchive's
`t/02-read.rakutest` gets from test 5 to test 19.
