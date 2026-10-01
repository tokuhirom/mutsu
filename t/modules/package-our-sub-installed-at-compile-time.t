use Test;

# An `our sub` declared in a package is installed when the compunit is
# compiled, wherever the package or the sub sits: in a never-run branch,
# inside an uncalled routine, or in a nested block of the package body
# (mutsu#10504).

plan 12;

is NeverRun::g(), 42, 'package in a never-run branch';
if False { package NeverRun { our sub g { 42 } } }

is InUncalled::g(), 43, 'package inside an uncalled routine';
sub never-called { package InUncalled { our sub g { 43 } } }

is DeadBranch::g(), 44, 'our sub in a never-run branch of a package body';
package DeadBranch { if False { our sub g { 44 } } }

# The sub closes over its declaring block: before that block runs it sees the
# lexical's undefined value, afterwards the bound one.
nok Capture::g().defined, 'nested-block lexical is undefined before the block runs';
package Capture { { my $x = 5; our sub g { $x } } }
is Capture::g(), 5, 'nested-block lexical is bound once the block has run';
{
    my $x = 9;
    is Capture::g(), 5, 'a later same-named lexical does not leak into the closure';
}

# A routine-nested package still binds each activation's parameter.
sub mk($v) { package PerCall { our sub g { $v } } }
mk(3);
is PerCall::g(), 3, 'routine-nested package sub sees the first activation';
mk(4);
is PerCall::g(), 4, 'routine-nested package sub sees the latest activation';

# An `our proto` with its `our multi` candidates in a nested block: the
# candidates are installed once, not duplicated by the in-sequence pass.
is Multi::h(1), 'int', 'nested our multi is callable before its block runs';
package Multi {
    if True {
        our proto h($) {*}
        our multi h(Int) { 'int' }
        our multi h(Str) { 'str' }
    }
}
is Multi::h('a'), 'str', 'nested our multi dispatches after its block ran';

# A plain (lexical) `sub` in a nested block stays private to that block.
package Lexical { if False { sub r { 2 } } }
dies-ok { Lexical::r() }, 'a lexical sub in a nested block is not installed';

# A package whose body the BEGIN prologue splits keeps its runtime half's
# routines too.
BEGIN { 1 }
is Split::g(), 45, 'our sub in the runtime half of a split package body';
package Split { my $y = 1; if False { our sub g { 45 } } }
