use v6;
use Test;

# A role mixed into a native-backed instance (via `does`/`but`) must still
# dispatch the instance's *native* methods to the inner value. Before the fix,
# the by-name native dispatchers (string/IO/etc.) intercepted the method on the
# `Base+{Role}` Mixin wrapper and hard-errored with "No such method" instead of
# delegating to the inner value. This is what HTTP::UserAgent needs: it mixes a
# `Connection` role onto an `IO::Socket::INET` and calls native `.print`/`.recv`.

plan 8;

role Marker { method marker { "marked" } }

# IO::Path is a native-backed instance. Mix a role in and call native methods.
my $path = "/foo/bar/baz.txt".IO;
$path does Marker;

is $path.marker,   "marked",  "role method works on mixed IO::Path";
is $path.basename, "baz.txt", "native basename delegates to inner IO::Path";
is $path.extension, "txt",    "native extension delegates to inner IO::Path";
is $path.parent.basename, "bar", "native parent chain works through the mixin";

# `but` form (single value mixin) on IO::Path.
my $p2 = "/a/b.md".IO but Marker;
is $p2.marker, "marked", "role method works via `but` on IO::Path";
is $p2.basename, "b.md", "native basename works via `but` on IO::Path";

# A user-declared method that shadows a native one still runs (with self bound to
# the wrapper) — the delegation must NOT swallow user overrides.
role Renamer { method basename { "OVERRIDDEN" } }
my $p3 = "/x/y.txt".IO but Renamer;
is $p3.basename, "OVERRIDDEN", "role method shadowing a native name wins";

# Allomorph-style value mixin still coerces natively (regression guard).
my $n = 5 but Marker;
is $n + 3, 8, "numeric mixin still does arithmetic natively";

done-testing;
