use Test;

# ADR-0019 C6d-1. A user-defined operator, a reduce/hyper step over one, and a
# `MAIN` candidate used to be invoked through the interpreter entry
# `call_function_def`, whose body run was `run_block(&def.body)` -- a fresh
# compile of the routine's AST on every call. They now run the routine's
# bytecode; `call_function_def` survives only for the gated case named below.
#
# Every expectation below was taken from `raku` first. The one shape that
# deliberately stays on the interpreter entry -- a state-bearing candidate of a
# signature-alternates name -- cannot be verified against `raku`, which does not
# accept that syntax; `t/multi-signature-alternates.t` pins it instead.

plan 26;

sub prefix:<dbl>($x) { $x * 2 }
sub postfix:<!>($n) { [*] 1..$n }
sub infix:<myop>($a, $b) { $a + $b * 2 }

is dbl 21, 42, 'user prefix operator';
is 5!, 120, 'user postfix operator';

# The reduce/hyper/cross/zip metaoperator forms over a user infix: each step
# resolves the candidate itself and then invokes it directly.
is ([myop] 1, 2, 3), 11, 'reduce over a user infix operator';
is (1, 2, 3).reduce(&infix:<myop>), 11, 'reduce with the operator as a routine value';
is (my @l = (1, 2, 3)) >>myop<< (my @r = (10, 20, 30)), (21, 42, 63), 'hyper over a user infix';
is (@l Xmyop @r).elems, 9, 'cross-metaop over a user infix';
is ((1, 2) Zmyop (10, 20)), (21, 42), 'zip-metaop over a user infix';
is ([\myop] 1, 2, 3), (1, 5, 11), 'triangular reduce over a user infix';

# A *multi* operator is the shape that actually reached the interpreter entry:
# `resolve_function_with_types` picks the candidate, so the invocation had no
# dispatch of its own left to do.
multi sub infix:<mm>(Int $a, Int $b) { $a * 10 + $b }
multi sub infix:<mm>(Str $a, Str $b) { "$a|$b" }

is 1 mm 2, 12, 'multi user infix, Int candidate';
is "a" mm "b", 'a|b', 'multi user infix, Str candidate';
is ([mm] 1, 2, 3), 123, 'reduce picks the Int candidate at every step';
is ([mm] "a", "b", "c"), 'a|b|c', 'reduce picks the Str candidate at every step';
is ([mm] 1, 2, 3, 4), 1234, 'a four-element reduce stays on one candidate';

# An `is rw` parameter must still write back through the invocation.
sub postfix:<!!>($n is rw) { $n = $n + 1; $n }
my $c = 4;
is $c!!, 5, 'rw parameter of a user postfix operator returns the new value';
is $c, 5, 'rw parameter of a user postfix operator wrote back';

# A `state` variable must belong to the routine, not to one invocation: the
# per-call recompile this slice removes was the mechanism that could hand each
# call a fresh cell.
sub infix:<ss>($a, $b) { state $n = 0; $n = $n + 1; "($a,$b,$n)" }
is ([ss] 1, 2, 3), '((1,2,1),3,2)', 'state in a reduced operator counts up within one reduce';
is ([ss] 4, 5), '(4,5,3)', 'state in a reduced operator persists across reduces';

# Declared defaults, and the routine's own body constructs.
sub infix:<dd>($a, $b = 100) { $a + $b }
is ([dd] 1, 2), 3, "a reduce supplies the operator's second argument";
is infix:<dd>(1), 101, "the operator's own default applies when the argument is absent";

sub infix:<blk>($a, $b) { my @acc; @acc.push($_) for $a, $b; @acc.join('-') }
is ([blk] 'x', 'y'), 'x-y', 'an operator body with its own block and loop';

# A return constraint is part of the routine, so it must survive whichever entry
# invokes it.
sub infix:<rt>($a, $b --> Int) { $a + $b }
is ([rt] 1, 2, 3), 6, 'a reduced operator honours its return constraint';

# A `where`-constrained multi operator: the caller resolves the candidate, so
# the narrowing must already have happened by the time the body runs.
multi sub infix:<wc>($a where * > 0, $b) { "pos $a $b" }
multi sub infix:<wc>($a, $b) { "other $a $b" }
is 1 wc 2, 'pos 1 2', 'where-constrained candidate of a user operator';
is -1 wc 2, 'other -1 2', 'the fallback candidate when the where fails';

# An exception must propagate out of the invocation, not be swallowed by it.
sub prefix:<boom>($x) { die "boom $x" }
my $err;
try { boom 3; CATCH { default { $err = .message } } }
is $err, 'boom 3', 'a die inside a user prefix operator propagates';

# The operator resolved from another package still runs in its defining package.
module Ops {
    our sub infix:<pk>($a, $b) is export { "{$?PACKAGE.^name}:{$a + $b}" }
}
is (Ops::infix:<pk>(1, 2)), 'Ops:3', 'a package-scoped operator keeps its defining package';

# `.candidates` on a multi operator still yields callable code objects.
is (&infix:<mm>.candidates.elems), 2, 'both candidates of a multi operator are visible';
