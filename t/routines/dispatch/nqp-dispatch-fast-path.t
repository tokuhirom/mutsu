use Test;

# The `nqp::` namespace is reserved, so mutsu dispatches an `nqp::` call
# straight to its op table instead of walking the whole routine-resolution
# chain (news/2026-09/nqp-and-name-dispatch-fast-paths.md). These tests pin the
# behaviour that short-circuit must not change: the arguments an op sees, the
# no-paren term form, and the fact that a user routine cannot capture the name.

plan 14;

# -- the call form still sees normalized arguments ---------------------------
# A bareword type object reaches the op decontainerized; skipping the call
# path's VarRef unwrapping made `eqaddr` answer 0 for two identical type
# objects.
is nqp::eqaddr(Int, Int), 1, 'nqp::eqaddr(Int, Int) is 1';
is nqp::eqaddr(Int, Str), 0, 'nqp::eqaddr(Int, Str) is 0';

my @a;
is nqp::eqaddr(@a, @a), 1, 'a variable argument keeps its identity';
is nqp::eqaddr([1], [1]), 0, 'two distinct containers are not identical';

my $s = 'abc';
is nqp::chars($s), 3, 'a `$`-variable argument arrives decontainerized';

# -- the no-paren term form --------------------------------------------------
# rakudo's own Test.rakumod writes `nqp::time` with no parentheses.
my $t = nqp::time;
ok $t ~~ Int, 'a 0-arg `nqp::` term without parens yields its value';
ok $t > 0, 'and nqp::time is a positive epoch reading';

# -- the interpreter-coupled ops still resolve -------------------------------
# These live in a different table from the pure value ops; both have to be
# reachable through the one dispatch entry point.
is nqp::sha1('abc'), 'A9993E364706816ABA3E25717850C26C9CD0D89D', 'nqp::sha1';
my %h = a => 1;
is nqp::atkey(%h, 'a'), 1, 'nqp::atkey';
is nqp::atpos([10, 20, 30], 1), 20, 'nqp::atpos';
is nqp::join('-', nqp::split(',', '1,2,3')), '1-2-3', 'nqp::join / nqp::split';

# -- a user routine cannot capture an `nqp::` name ---------------------------
# The short-circuit assumes the namespace is reserved. A same-named ordinary
# routine must therefore leave the op alone, and keep working itself.
sub join(*@parts) { 'user-join' }
is join('x', 'y'), 'user-join', 'a user `join` still wins for the bare name';
is nqp::join('-', nqp::split(',', '4,5')), '4-5', 'but nqp::join is untouched';

# -- an op mutsu does not implement still fails loudly -----------------------
# Silently reaching Raku's same-named builtin would return a different value
# (`nqp::index` yields -1 where `index` yields Nil) and nqp code branches on it.
dies-ok { EVAL 'nqp::no_such_op_at_all(1)' }, 'an unimplemented nqp:: op dies';
