use Test;

# GH #8064. A user-declared `only` sub hides a same-named CORE routine
# COMPLETELY: a call whose arguments do not fit the declared signature is a
# binding failure, never a silent call to the builtin. Both dispatch chains
# used to decide "the user sub does not accept these arguments" and then hand
# the call to the native tables, so the binding failure disappeared -- most
# visibly through `dies-ok`, which reported such a block as having lived.
#
# The two gates that got this wrong keyed on `BUILTIN_FUNCTION_NAMES`, which
# is much narrower than the native arity tables: `pick` is answered by
# `builtins::native_function` without appearing in that list at all, which is
# why this shape survived the earlier builtin-shadow work pinned by
# `builtin-shadow-dispatch.t`.

plan 10;

sub pick(::T $x, T $y) { "user:$x/$y" } #OK shadow

# A type capture resolves `T` from `$x`, so `$y` must be an Int here.
dies-ok { pick(1, 'two') }, 'capture-constrained mismatch dies inside dies-ok';
dies-ok({ pick(1, 'two') }), 'the same, written with parens';
lives-ok { pick(1, 2) }, 'a call that does fit the capture still lives';
is pick(1, 2), "user:1/2", 'and reaches the user sub, not CORE pick';

throws-like { pick(1, 'two') }, X::TypeCheck::Binding::Parameter,
    'the mismatch is a parameter binding failure';

# The same for a plainly typed shadow: no type capture is needed to reach it.
sub roll(Int $x, Int $y) { "user:$x/$y" } #OK shadow
dies-ok { roll(1, 'two') }, 'plainly typed shadow of a native-table name dies';
is roll(1, 2), "user:1/2", 'the matching call reaches the user sub';

# An arity mismatch is a binding failure too, not a call to CORE's routine.
dies-ok { roll(1) }, 'too few positionals is a binding failure, not CORE roll';

# A `multi`, unlike an `only` sub, ADDS candidates to the core ones rather
# than hiding them, so a call no user candidate accepts still reaches CORE
# (verified against rakudo: `multi sub min(Int $a) {...}; min(3, 4)` is 3).
multi sub min(Int $a) { "user:$a" } #OK shadow
is min(3), "user:3", 'the user multi candidate wins when it matches';
is min(3, 4), 3, 'a call it does not match still reaches the core candidate';
