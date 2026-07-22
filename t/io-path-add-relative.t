use Test;

# IO::Path.add is slurpy (*@children): it flattens its positional arguments and
# joins each element onto the path as a separate segment. IO::Path.relative
# defaults its base to $*CWD (not the receiver's own .CWD, unlike .absolute).

plan 14;

# --- .add: slurpy / list flattening ---------------------------------------
is "foo".IO.add(<bar baz>).Str, "foo/bar/baz",
    '.add(<bar baz>) splits the word list into path segments';
is "foo".IO.add("a", "b").Str, "foo/a/b",
    '.add with multiple positional args joins each as a segment';
is "foo".IO.add(("a", ("b", "c"))).Str, "foo/a/b/c",
    '.add deep-flattens a nested list';
is "foo".IO.add("bar baz").Str, "foo/bar baz",
    '.add with a single Str keeps it whole (no split on space)';
is "foo".IO.add().Str, "foo",
    '.add() with no arguments is a no-op';

# --- .child: single Str() child (NOT slurpy) ------------------------------
is "foo".IO.child(<bar baz>).Str, "foo/bar baz",
    '.child(<bar baz>) coerces the list to a single Str child';
is "foo".IO.child("x").Str, "foo/x",
    '.child adds a single segment';

# --- .relative: default base is $*CWD -------------------------------------
is "foo/bar".IO.resolve.relative, "foo/bar",
    '.resolve.relative relativizes against $*CWD, not the stamped :CWD("/")';
is IO::Path.new("/a/b/c", :CWD("/a")).relative, "/a/b/c".IO.relative,
    '.relative ignores the receiver .CWD and uses $*CWD';

# a plain relative receiver's .relative round-trips against $*CWD
is "sub/dir/file".IO.relative, "sub/dir/file",
    '.relative of a cwd-relative path is itself';

# explicit base still honored (the zef $archive.relative($tmp) path)
is "/a/b/c".IO.relative("/a"), "b/c",
    '.relative(base) strips an explicit ancestor base';
is "/a/x".IO.relative("/a/b"), "../x",
    '.relative(base) walks up with .. for a sibling base';

# --- .absolute: default base is the receiver .CWD (asymmetric with .relative)
is IO::Path.new("b/c", :CWD("/a")).absolute, "/a/b/c",
    '.absolute defaults to the receiver .CWD';
is "/x/y".IO.absolute, "/x/y",
    '.absolute of an already-absolute path is itself';
