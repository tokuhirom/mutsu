my constant &path-exists = -> str $_ { "a$_" };
my sub path-is-text(str $x) { "b$x" }
my sub EXPORT(*@names) {
    Map.new: @names
      ?? @names.map: {
             if UNIT::{"&$_"}:exists { UNIT::{"&$_"}:p }
             else {
                 my ($in,$out) = .split(':', 2);
                 if $out && UNIT::{"&$in"} -> &code { Pair.new: "&$out", &code }
             }
         }
      !! UNIT::.grep: { .key.starts-with('&') && .key ne '&EXPORT' }
}
