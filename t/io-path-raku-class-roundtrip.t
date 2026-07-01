use Test;

# `IO::Path.raku` used to derive its rendered class name from the `SPEC`
# attribute (e.g. `IO::Spec::Unix` -> `IO::Path::Unix`) instead of using the
# instance's actual class. A plain `"str".IO` is class `IO::Path` (its `.^name`
# says so) even though its `$*SPEC` default happens to be `IO::Spec::Unix` on
# this platform, so `.raku` rendered a class name that DIFFERED from `.^name`
# -- and `.raku.EVAL` on that string built an `IO::Path::Unix` instance, which
# is a different `class_name` than the original and so never `eqv`-equal to
# it (Instance equality includes `class_name`).

plan 3;

my $p = "tmp/io-path-raku-roundtrip-test.txt".IO;
is $p.raku.EVAL.^name, $p.^name, '.raku renders the actual instance class, not a SPEC-derived name';
ok $p.raku.EVAL eqv $p, '.raku.EVAL round-trips eqv to the original for a plain IO::Path';

# An explicitly-subclassed instance must still round-trip to itself.
my $w = IO::Path::Win32.new("foo/bar.txt", :CWD("/tmp"));
ok $w.raku.EVAL eqv $w, '.raku.EVAL round-trips eqv for an explicit SPEC-variant subclass';
