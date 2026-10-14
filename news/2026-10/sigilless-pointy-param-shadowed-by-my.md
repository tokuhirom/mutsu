# A `for` pointy sigilless parameter survives a later `my $name` in its body

`for [[1, 2],] -> \a { my $a = 45; say a.WHAT }` now prints `(Array)` as in Rakudo. The
loop's sigilless parameters are registered as declared bindings of the body, so the later
scalar declaration parks them under the term key instead of overwriting them (#13474).
