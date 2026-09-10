use Test;

# Pure lexical IO::Path methods (`.parent`/`.add`/`.basename`/`.sibling`/
# `.cleanup`/`.extension`/`.volume`/`.succ`/`.pred`/`.starts-with`/
# `.is-absolute`/`.is-relative`/`.Str`/`.gist`/`.IO`) are dispatched natively by
# the bytecode VM (no filesystem / cwd / env), sharing the single
# `try_io_path_lexical` impl the interpreter's `native_io_path` delegates to.
# These derive a new path/string/bool purely from the receiver's attributes, so
# both a literal `.IO` receiver and a variable receiver (the mut dispatch path)
# must agree with Rakudo.

plan 40;

# --- literal `.IO` receiver (non-mut dispatch path) ---
is "/foo/bar/baz".IO.parent.Str,        "/foo/bar",     "parent (literal)";
is "/foo/bar/baz".IO.parent(2).Str,     "/foo",         "parent(2)";
is "/foo/bar/baz".IO.parent(0).Str,     "/foo/bar/baz", "parent(0) is identity";
is "/foo/bar".IO.add("qux").Str,        "/foo/bar/qux", "add";
is "/foo/bar".IO.child("c").Str,        "/foo/bar/c",   "child (non-secure)";
is "/foo/bar/baz".IO.basename,          "baz",          "basename";
is "/foo/bar/baz".IO.dirname,           "/foo/bar",     "dirname";
is "/foo/bar/baz".IO.sibling("s").Str,  "/foo/bar/s",   "sibling";
is "rel/path".IO.parent.Str,            "rel",          "parent of relative";

# --- variable receiver (mut dispatch path) ---
my $p = "/a/b/c.tar.gz".IO;
is $p.parent.Str,         "/a/b",         "parent (var)";
is $p.add("d").Str,       "/a/b/c.tar.gz/d", "add (var)";
is $p.basename,           "c.tar.gz",     "basename (var)";
is $p.dirname,            "/a/b",         "dirname (var)";
is $p.sibling("e").Str,   "/a/b/e",       "sibling (var)";
is $p.extension,          "gz",           "extension (var)";
is $p.cleanup.Str,        "/a/b/c.tar.gz","cleanup (var)";
ok $p.is-absolute,                        "is-absolute (var)";
nok $p.is-relative,                       "is-relative (var)";
ok $p.starts-with("/a"),                  "starts-with true";
nok $p.starts-with("/z"),                 "starts-with false";

# the variable is not mutated by these pure derivations
is $p.Str, "/a/b/c.tar.gz", "receiver variable unchanged after derivations";

# --- extension replacement / removal ---
is "foo.tar.gz".IO.extension("tgz").Str, "foo.tar.tgz", "extension replace one part";
is "foo.tar.gz".IO.extension("", :parts(0..*)).Str, "foo", "extension strip all";
is "foo".IO.extension,    "",             "extension of extensionless is empty";

# --- cleanup normalizes lexically (collapses `//` and `/./`, keeps `..`) ---
is "/a/b//c".IO.cleanup.Str, "/a/b/c",    "cleanup collapses duplicate slashes";
is "/a/./b".IO.cleanup.Str,  "/a/b",      "cleanup removes single-dot segments";

# --- succ / pred on the basename ---
is "abc".IO.succ.Str,     "abd",          "succ";
is "abd".IO.pred.Str,     "abc",          "pred";
is "/x/y/az".IO.succ.Str, "/x/y/ba",      "succ carries within basename only";

# --- volume (empty on POSIX) ---
is "/foo/bar".IO.volume,  "",             "volume empty on POSIX";

# --- relative path predicates ---
ok "a/b".IO.is-relative,                  "relative path is-relative";
nok "a/b".IO.is-absolute,                 "relative path not is-absolute";

# --- gist / Str / IO round-trip ---
is "/foo".IO.Str,         "/foo",         "Str";
is "/foo".IO.gist,        '"/foo".IO',    "gist of absolute path";
ok "/foo".IO.IO ~~ IO::Path,              ".IO on an IO::Path is an IO::Path";
is "/foo".IO.IO.Str,      "/foo",         ".IO preserves the path";

# --- chained derivations stay pure ---
is "/a/b/c".IO.parent.add("x").Str, "/a/b/x", "chained parent.add";
is "/a/b/c".IO.sibling("d").basename, "d",    "chained sibling.basename";

# --- subclass round-trips through path-deriving methods ---
my $w = IO::Path::Win32.new("a\\b\\c");
ok $w.parent ~~ IO::Path::Win32,          "Win32 parent stays Win32";
is $w.parent.Str,         "a\\b",         "Win32 parent uses backslash sep";
