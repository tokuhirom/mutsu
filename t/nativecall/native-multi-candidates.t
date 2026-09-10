use Test;

# Native type names are dispatch constraints, not aliases for their boxed
# counterparts.  A boxed lexical must not make a native-only candidate match,
# while a native lexical can still fall back to a boxed candidate.

plan 7;

multi sub native-only-int(int $value) { $value }

my Int $boxed = 2;
my int $native = 2;

throws-like { native-only-int($boxed) }, X::Multi::NoMatch,
    'a boxed Int does not match a native-only candidate';
is native-only-int($native), 2,
    'a native lexical matches a native candidate';

multi sub native-widths(int $value) { 'int' }
multi sub native-widths(int32 $value) { 'int32' }

throws-like { native-widths($native) }, X::Multi::Ambiguous,
    'native widths in one family are ambiguous';

multi sub native-or-boxed(int $value) { 'native' }
multi sub native-or-boxed(Int $value) { 'boxed' }

is native-or-boxed($boxed), 'boxed',
    'a boxed lexical selects the boxed candidate';
is native-or-boxed($native), 'native',
    'a native lexical selects the native candidate';
is native-or-boxed(42), 'native',
    'a native-shaped literal selects the native candidate';

my Int $boxed-again = 3;
throws-like { native-only-int($boxed-again) }, X::Multi::NoMatch,
    'boxed values stay boxed across repeated dispatches';

done-testing;
