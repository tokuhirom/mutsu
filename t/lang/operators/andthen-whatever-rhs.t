use Test;

# A Whatever-curried right-hand side of `andthen` / `orelse` / `notandthen`
# is called with the left operand, like a block literal (rakudo 2026.11,
# RakuAST). A bare `*` is not curried and stays the value `*`.
# Found via App::Moneymoor's t/65-main-shell.rakutest, which looks a tab up
# with `(TABS.first({ ... }) andthen *<label>) // ''`.

plan 11;

my %h = label => 'L';
is (%h andthen *<label>), 'L', 'andthen calls a *<key> RHS with the LHS';
is (5 andthen * + 1), 6, 'andthen calls a * + 1 RHS';
is (5 andthen *.succ), 6, 'andthen calls a *.method RHS';
is (5 andthen (* + 1)), 6, 'a parenthesized WhateverCode RHS is called';
is-deeply (Any andthen *<label>), Empty, 'an undefined LHS skips the RHS';
is (Any orelse *.raku), 'Any', 'orelse calls a WhateverCode RHS';
is (Int notandthen *.raku), 'Int', 'notandthen calls a WhateverCode RHS';
is (Int notandthen -> $x { "got " ~ $x.raku }), 'got Int', 'notandthen calls a pointy block RHS';
is (Int notandthen { "b " ~ $_.raku }), 'b Int', 'notandthen calls a bare block RHS';
ok (5 andthen *) ~~ Whatever, 'a bare * RHS is not called';
my &f = * + 2;
ok (5 andthen &f) ~~ WhateverCode, 'a &var holding a WhateverCode is not called';
