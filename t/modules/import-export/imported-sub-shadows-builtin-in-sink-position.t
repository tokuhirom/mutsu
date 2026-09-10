use lib $?FILE.IO.parent.add('lib').Str;
use BuiltinShadow;
use Test;

plan 4;

# An imported routine shadows a same-named builtin at EVERY call site. mutsu
# compiles a call in sink (non-final) position to the `ExecCall` opcode, whose
# fallback tried the builtin first and only reached user dispatch when the name
# was not a builtin at all — so a shadowed name in sink position could silently
# reach the builtin, while the same call in final position (compiled to
# `CallFunc`, which has always honoured the shadow) worked.
#
# `Cro::HTTP::Router` exports `get`, and mutsu's builtin `get` reads a line from
# a handle: a `route` block whose `get -> { ... }` sat anywhere but last died
# with "Expected IO::Handle".
#
# NOTE: these checks pass on the commit that introduced them AND on its parent —
# no synthetic arrangement found so far pushes a shadowed call onto the
# `ExecCall` path (several are recorded in the ticket this fix closes). They
# characterise the rule the fix restores rather than reproducing the failure;
# the reproducers that DO fail without the fix are `tmp/st6q.p6` and the
# vendored Cro suite's `http-middleware.rakutest` subtest 6, both of which need
# the Cro tree.

# Final position: the call whose value the block returns.
shadow-reset;
{
    get('final', -> { 1 });
}
is shadow-calls(), 'get:final', 'an imported sub shadows the builtin in final position';

# Sink position: a call whose value is discarded.
shadow-reset;
{
    get('sink', -> { 1 });
    my $unused = 1;
}
is shadow-calls(), 'get:sink', 'an imported sub shadows the builtin in sink position';

# Two shadowed calls in a row: the first is in sink position, the second final.
shadow-reset;
{
    get('one', -> { 1 });
    lines('two', -> { 1 });
}
is shadow-calls(), 'get:one,lines:two', 'both shadowed names win, whatever their position';

# The shape that actually broke: the shadowed call sits in sink position inside
# a block the module itself invokes, and the whole thing runs inside a `subtest`
# (whose body is compiled at run time, which is what pushed the call onto the
# `ExecCall` path).
subtest {
    is shadow-runner({
        get('one', -> { 1 });
        lines('two', -> { 1 });
    }), 'get:one,lines:two',
        'a shadowed call in sink position wins inside a module-invoked block';
}, 'inside a subtest';
