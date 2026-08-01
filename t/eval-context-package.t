use v6;
use Test;
use lib 't/lib';
use EvalContext;

plan 6;

# `EVAL $code, context => $ctx` compiles the string as if it stood at `$ctx`'s
# frame, so a package the snippet declares belongs to the caller's package --
# not to the module that happened to call EVAL. rakudo's own `Test.rakumod`
# relies on this for the string form of `throws-like`.

# A plain EVAL from inside a module compiles in the module's package.
is run-plain('my class Foo { }; Foo.^name'), 'EvalContext::Foo',
    'a plain EVAL names the snippet class after the calling module';

# With `context => CALLER::` it compiles in the caller's package instead.
is run-with-caller-context('my class Foo { }; Foo.^name'), 'Foo',
    'context => CALLER:: names the snippet class after the caller';

# The context survives being stored and used several frames deeper, which is
# exactly what `throws-like` does (it takes CALLER:: in its own body and EVALs
# inside a nested block).
is run-context-through-block('my class Bar { }; Bar.^name'), 'Bar',
    'a stored CALLER:: context still names the caller package deeper down';

# The caller here is a routine of this script, not the mainline.
sub outer() { run-with-caller-context('my class Baz { }; Baz.^name') }
is outer(), 'Baz', 'context => CALLER:: from a script routine';

# The context must not leak: the next plain EVAL is back in the module.
is run-plain('my class Qux { }; Qux.^name'), 'EvalContext::Qux',
    'the context does not leak into a later plain EVAL';

# An error raised while compiling the snippet names the caller package too.
throws-like { run-with-caller-context('my class Zonk { method m() { $!nope } }') },
    Exception,
    message => /'class Zonk'/,
    'a compile error from the snippet names the unqualified class';
