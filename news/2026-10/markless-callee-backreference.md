# A `:m` callee's backreference compares exactly, as rakudo does

A whole-`:m` rule called as `<r>` over a subject that has combining marks now
runs as a markless frame even when it holds a backreference (`$0`, `$<k>`) or a
capture marker (`<(`, `)>`). Before, such a body declined the markless compile
and took the eager `:m` path, which matched the stripped subject: a
backreference there ignored marks (`(a+) $0` matched `"áa"`), where rakudo
compares the captured text exactly, marks included. The markless frame
compares over the subject's own text, so the callee now agrees with rakudo and
enters its ends one at a time (#13176).

A whole-pattern `:m` match (`~~ /:m (a) $0/`) still compares stripped text;
that is #13201.
