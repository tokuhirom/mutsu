use Test;

# `EVAL $code, context => $ctx` compiles the snippet as if it stood at `$ctx`'s
# frame, so it must inherit the *caller's* import visibility — not that of the
# module which happens to call EVAL. The vendored `Test.rakumod`'s string form
# of `throws-like` is exactly this shape (`EVAL $code, context => CALLER::`),
# and without it no symbol the test file imported was resolvable from the
# snippet: `Encode/t/01-basic.t` reported `X::AdHoc: Could not find symbol
# 'Encode::decode'` instead of the encoding exception it asserts on (#7837).
#
# The `use-ok` shape at the end is the other half of the same ticket: an
# `EVAL "use ..."` reached from inside another compunit's routine used to stamp
# every routine the loaded module declares with the *calling* module's file,
# which then anchored that module's own `sub EXPORT` — and its `END` — to a
# compunit that never named it.

plan 6;

use lib 't/lib';
use EvalCtxHelper;
use EvalCtxTarget;

# Sanity: the qualified names really are visible here, in the importing file.
is EvalCtxTarget::ping(1), 'pong:1', 'the test file can call the imported module qualified';

is eval-in-caller('EvalCtxTarget::ping(2)'), 'pong:2',
    'EVAL with a CALLER:: context resolves a sub the CALLER imported';
is eval-in-caller('EvalCtxTarget::Marker.new.label'), 'marker',
    'EVAL with a CALLER:: context resolves a class the CALLER imported';

# The context argument is the whole difference: without it the snippet compiles
# in EvalCtxHelper's own compunit, which never imported EvalCtxTarget. Rakudo
# fails here too ("Could not find symbol '&ping' in 'GLOBAL::EvalCtxTarget'").
dies-ok { eval-no-context('EvalCtxTarget::ping(3)') },
    'without a context the snippet does not see the caller compunit\'s imports';

# A caller-context EVAL still reaches the caller's own routines. (It does NOT
# reach the caller's `my` lexicals — rakudo reports those undeclared, since
# `CALLER::` is a snapshot rather than the live pad — so that is not asserted.)
sub caller-local() { 'local-sub' }
is eval-in-caller('caller-local()'), 'local-sub',
    'EVAL with a CALLER:: context calls a sub declared in the caller';

# `use-ok`'s shape: `EVAL "use M"` from inside a module's routine. The loaded
# module's own `sub EXPORT` names its own package qualified, and so does its
# `END` phaser — the latter running long after every compunit that could vouch
# for it is off the stack, which is why a failure there shows up as a non-zero
# exit status rather than as a failed assertion here.
is eval-use('EvalCtxSelfRef'), 'loaded',
    'EVAL "use M" from inside a module runs M\'s own qualified-self-referencing EXPORT';
