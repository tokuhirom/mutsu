# A comment is not code: a module named only in prose must not be loaded.
#
# mutsu gates several builtin preludes on the compunit's source *mentioning* a
# name (`NativeCall`, `does IO::Socket`, `trait_mod:<does>`, ...). Those gates
# used to read the raw source, so `use NativeCall` written in a COMMENT was
# enough to inject NativeCall's exported helpers at the top level: the routines
# below became resolvable in a program that never asked for them, and a `t/`
# pin whose header discussed a provider silently stopped testing what it
# existed to test (GH #7611).
#
# So the sentences in this header are the fixture. `use NativeCall;` appears
# above as prose, `cglobal`, `nativecast`, `nativesizeof` and
# `explicitly-manage` are all named here, and none of them may be visible.
use Test;

plan 6;

nok defined(::('&nativecast')),
    'a provider named only in a comment does not register its exported routines';
nok defined(::('&cglobal')),
    'the same for every routine the prose names, not just the first';
nok defined(::('&nativesizeof')),
    'and for a routine named on its own line of prose';

# The same for Pod, which is prose too.

=begin pod

This block mentions C<use NativeCall> and C<explicitly-manage> the way any
documented module would.

=end pod

nok defined(::('&explicitly-manage')),
    'a provider named only in a Pod block does not register its routines either';

# A `#` inside a string is not a comment, so the code around it must survive
# the prose-stripping pass unchanged.
is "# not a comment", '# not a comment', 'a hash inside a string is code';

# And a comment naming a routine still does not conjure it after real code has
# run: the gate is decided once, for the whole compunit.
# nativecast() nativesizeof() cglobal()
nok defined(::('&nativecast')),
    'still not visible after the mainline has started';
