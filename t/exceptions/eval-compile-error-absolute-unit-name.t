use Test;

# rakudo compiles an EVAL (or `Str.AST`) unit under its name joined onto
# `$*CWD`, while the unit's own `$?FILE` stays the bare `EVAL_<N>`: a compile
# error's `.filename` is the absolute name.

plan 4;

try EVAL '1 +';
like $!.filename, /^ $($*CWD) '/EVAL_' \d+ $/, 'an EVAL compile error: .filename under $*CWD';
try Q[my $x = 1 2].AST;
like $!.filename, /^ $($*CWD) '/EVAL_' \d+ $/, 'a Str.AST parse failure: the same';
like EVAL(q[$?FILE]), /^ 'EVAL_' \d+ $/, '$?FILE stays the bare unit name';
try EVAL '1 +', :filename</explicit/name.raku>;
is $!.filename, '/explicit/name.raku', 'an explicit absolute :filename is kept';
