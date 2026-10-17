use Test;

# A program-scope typed scalar that a named sub captures is checked when the
# sub stores into it by a run-time name (`EVAL`, a symbolic lookup), not only
# when the store is compiled in place (#13746). Checked against rakudo.

plan 8;

my Int $n = 1;
sub set-n($v) { EVAL q[$n = $v] }
throws-like { set-n('no') }, X::TypeCheck::Assignment,
    message => /'expected Int but got Str'/,
    'an EVAL\'d store in a sub checks the program variable\'s type';
set-n(Nil);
is $n.raku, 'Int', 'an EVAL\'d Nil resets it to its type object';
set-n(5);
is $n, 5, 'a matching value is stored';

my Int $m = 1;
sub set-m() { ::('$m') = 'no' }
throws-like { set-m() }, X::TypeCheck::Assignment,
    'a symbolic store in a sub checks the type';

my Int $d is default(7) = 1;
sub set-d() { EVAL q[$d = Nil] }
set-d();
is $d, 7, 'an EVAL\'d Nil restores the default';

subset Even of Int where * %% 2;
my Even $e = 2;
sub set-e($v) { EVAL q[$e = $v] }
throws-like { set-e(3) }, X::TypeCheck::Assignment, 'a subset constraint holds';
set-e(4);
is $e, 4, 'a value matching the subset is stored';

my Int @a;
sub push-a() { EVAL q[@a.push('no')] }
throws-like { push-a() }, X::TypeCheck, 'an EVAL\'d push checks the element type';
