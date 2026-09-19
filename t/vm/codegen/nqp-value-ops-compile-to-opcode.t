use Test;

# An `nqp::` VALUE op is a compiler-known primitive in a reserved namespace, so
# it compiles to `OpCode::NqpOp` with the op resolved to a dense id at compile
# time instead of to a `CallFunc` that re-derives it from the callee string on
# every execution (#8817). These tests pin what that opcode has to keep doing:
# the operand preparation the call path used to perform, the ops reachable in
# each of the six dispatch tables, and the two shapes that deliberately stay on
# the old `CallFunc` path.

plan 25;

# -- operands arrive as plain values ----------------------------------------
# The call path wrapped every argument in a `VarRef` and unwrapped it again
# before dispatch; the opcode compiles operands as ordinary expressions. Both
# must hand the op the same thing.
my $s = 'abc';
is nqp::chars($s), 3, 'a `$` variable operand arrives decontainerized';

my @a;
is nqp::eqaddr(@a, @a), 1, 'an `@` variable operand keeps its identity';
is nqp::eqaddr(Int, Int), 1, 'a bareword type object is the same object twice';
is nqp::eqaddr(Int, Str), 0, 'and two different type objects are not';

my $n = 7;
is nqp::add_i($n, 1), 8, 'a literal operand mixes with a variable one';
is nqp::add_i(nqp::mul_i(3, 4), nqp::neg_i(2)), 10, 'nested ops nest';

# A `Proxy` operand is FETCHed, as it was by the call path's auto-fetch.
my $backing = 20;
my $proxy := Proxy.new(FETCH => sub ($) { $backing }, STORE => sub ($, $v) { $backing = $v });
is nqp::add_i($proxy, 1), 21, 'a Proxy operand is FETCHed before the op sees it';

# -- every dispatch table stays reachable ------------------------------------
# The id records which of the six chained tables owns an op so dispatch can
# enter it directly. One op per table, so a mis-tagged entry shows up here.
is nqp::atkey({ a => 1 }, 'a'), 1, 'the interpreter-coupled table (atkey)';
is nqp::iseq_i(4, 4), 1, 'the pure value table (iseq_i)';
is nqp::gethostname().chars > 0, True, 'the process table (gethostname)';
is nqp::ordat('hello', 1), 101, 'the text table (ordat)';
is nqp::index('hello', 'll'), 2, 'the string table (index)';
is nqp::elems(nqp::list(1, 2, 3)), 3, 'the list table (list) and elems';

# -- ops that mutate through an operand --------------------------------------
# These reach their storage through the shared container behind the value, not
# through any call-path argument wrapper, so they must still be visible after.
my @m = 1, 2, 3;
nqp::bindpos_i(@m, 1, 99);
is @m[1], 99, "nqp::bindpos_i writes through to the caller's array";
nqp::push(@m, 4);
is @m.elems, 4, "nqp::push appends to the caller's array";

# -- the control-flow forms are still special forms --------------------------
# They must NOT become value ops: their operands are lazy.
my $ran = 0;
my $if = nqp::if(1, 'then', do { $ran = 1; 'else' });
is $if, 'then', 'nqp::if still yields the taken branch';
is $ran, 0, 'and still does not evaluate the untaken one';

my $count = 0;
nqp::while(nqp::islt_i($count, 3), ($count = nqp::add_i($count, 1)));
is $count, 3, 'nqp::while still loops with a re-evaluated condition';

# `nqp::ifnull` is BOTH a lazy special form and a value-table op name; the
# special form has to keep winning at the call site.
my $fresh = 0;
is nqp::ifnull(5, do { $fresh = 1; 9 }), 5, 'nqp::ifnull yields the non-null arm';
is $fresh, 0, 'and leaves the fallback unevaluated';

# -- the shapes that stay on the CallFunc path -------------------------------
# An unknown name in the reserved namespace must keep failing loudly rather
# than reaching a same-named Raku builtin with different semantics.
dies-ok { EVAL 'nqp::no_such_op_at_all(1)' }, 'an unregistered nqp:: op still dies';
dies-ok { EVAL 'nqp::islt_i(1, 2); nqp::no_such_op_either(1)' },
        'an unregistered op dies even after a registered one compiled fine';

# A `|EXPR` spread is not a fixed-arity operand list, so it keeps the general
# call path, which is the only one that can spread it.
my @spread = 2, 3;
is nqp::add_i(|@spread), 5, 'a |EXPR operand list still spreads';

# -- the 0-arg term form -----------------------------------------------------
# `nqp::time` with no parentheses is a term, not a call; rakudo's own
# Test.rakumod writes it that way.
ok nqp::time > 0, 'a 0-arg nqp:: term without parens still yields its value';
ok nqp::time() > 0, 'and with parens too';
