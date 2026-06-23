use Test;

# Pin for the §D ③ ctor fork: `Capture.new`. Previously mutsu errored with
# "Unknown method ... new on Capture" — `Capture` had no constructor at all.
#
# raku semantics (verified): the default `Capture.new` produces an *empty*
# Capture. Named arguments are silently dropped (Capture has no buildable public
# attributes, so `bless` ignores them) and positional arguments are rejected
# ("Default constructor for 'Capture' only takes named arguments"), exactly as
# `Mu.new` (named-only) does. A *populated* Capture is built with `\(...)`.
#
# This is pure data assembly, so the VM builds it directly via
# `try_native_builtin_construct` (no env / registry / user code).

plan 9;

# Empty capture
my $c = Capture.new;
isa-ok $c, Capture, 'Capture.new yields a Capture';
is $c.elems, 0, 'Capture.new is empty';
is $c.list.elems, 0, 'Capture.new has no positional';
is $c.hash.elems, 0, 'Capture.new has no named';

# Named arguments are dropped (NOT stored as named captures)
my $n = Capture.new(:a(3), :b(4));
is $n.elems, 0, 'named args to Capture.new are dropped';
is $n<a>, Nil, 'a dropped named arg is not accessible';

# Positional arguments are rejected
dies-ok { Capture.new(1, 2) }, 'positional args to Capture.new die';
dies-ok { Capture.new("a" => 1) }, 'a positional Pair to Capture.new dies';

# A populated Capture is built with the `\(...)` literal (unchanged)
my $cap = \(1, 2, :a(3));
is $cap.list.elems, 2, '\\(...) still builds a populated Capture';
