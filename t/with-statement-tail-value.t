use v6;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';
use Test;
use WithTailVar;

# A statement-position `with`/`given` block always nets exactly one stack
# value in the VM, and statement-position compilers pop it — so it must never
# shadow the real tail value of the enclosing block. The module-loaded case
# runs through eval_block_value (stack.last()), which is where the leak bit
# (todo/tickets/module-loaded-sub-with-tail-var.md).

plan 13;

# Module-loaded subs (the original DBIish / NativeHelpers::Blob shape).
is with-then-tail(5), 42, 'use-loaded sub: with-block value does not clobber tail var';
is given-then-tail(5), 42, 'use-loaded sub: given-block value does not clobber tail var';

# Sub-value dispatch (eval_block_value path without modules).
sub g($p) { my $b = 7; with $p { 2.so; }; $b }
is-deeply (1,).map(&g).list, (7,), 'with before tail var via &sub in map';

is EVAL(q[my $b = 7; with 1 { 2.so; }; $b]), 7, 'with before tail var in EVAL';

# The value-position forms must still yield the block value.
sub tail-with($p) { with $p { 42 } }
is tail-with(5), 42, 'tail with still yields its block value';
nok tail-with(Any).defined, 'tail with on undefined yields an undefined value';

# Nested inside an if-branch (statement position inside a branch).
sub branchy($p) { my $b = 5; if True { with $p { 2.so; } }; $b }
is branchy(1), 5, 'with inside if-branch does not leak past the branch';

# Multiple withs in sequence must not pile up either.
sub multi-with($p) { my $b = 9; with $p { 1.so; }; with $p { 2.so; }; $b }
is multi-with(1), 9, 'two statement withs before the tail var';

# LEAVE + tail `with`/`if` (the NativeHelpers::Blob str-to-blob shape): the
# phaser block's tail statement must yield its branch value, not `True`.
is leave-then-tail-with(5), 42, 'module sub with LEAVE: tail with yields the branch value';
is leave-then-tail-if(5), 42, 'module sub with LEAVE: tail if yields the branch value';

# A `with` branch whose tail is a call to an imported sub (parsed as a
# statement call) still yields the call's return value.
is with-tail-imported-named(5), 'helper(5,3)',
    'with-branch tail: imported sub with named args yields its value';
is with-tail-imported-pos(5), 'helper-pos(5)',
    'with-branch tail: imported sub with positional args yields its value';

# The do-block (value) form of the phaser scope: tail if yields the branch.
sub do-leave-if($p) { do { LEAVE { 1 }; if $p.defined { "branch" } else { "e" } } }
is do-leave-if(5), 'branch', 'do-block with LEAVE: tail if yields the branch value';
