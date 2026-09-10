use Test;

# Path-deriving methods on an IO::Path SPEC-variant subclass must round-trip
# the concrete class (IO::Path::Unix / ::Win32 / ::Cygwin / ::QNX), not collapse
# it to the base IO::Path. Rakudo preserves the subclass, and is-deeply / eqv
# compares it — so `IO::Path::Win32.new("x").parent(0)` must stay IO::Path::Win32.

plan 10;

for IO::Path::Unix, IO::Path::Win32, IO::Path::Cygwin, IO::Path::QNX -> $cls {
    my $p = $cls.new("foo/bar/ber.txt", :CWD("/tmp"));
    is $p.parent(0).^name, $cls.^name, "{$cls.^name}.parent(0) keeps class";
    ok $p.parent(0) eqv $p, "{$cls.^name}.parent(0) eqv self";
}

# .sibling / .child also preserve the subclass.
{
    my $p = IO::Path::Win32.new("foo/bar", :CWD("/tmp"));
    is $p.sibling("baz").^name, "IO::Path::Win32", ".sibling keeps subclass";
    is $p.child("baz").^name,   "IO::Path::Win32", ".child keeps subclass";
}
