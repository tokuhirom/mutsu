use Test;

# `.IO` coercion over a Cool scalar (`"path".IO`, `42.IO`) is dispatched natively
# by the bytecode VM, constructing an IO::Path via the single shared
# `make_io_path_instance` the interpreter also uses (so `$*SPEC`/`$*CWD` are
# honored identically). An IO::Path(-subclass) *type object* returns itself.
# Instance / non-IO Package / aggregate / Junction receivers fall through to the
# interpreter (autothreading etc.), so behavior is unchanged.

plan 18;

# --- Str receiver (literal = non-mut path, variable = mut path) ---
is "abc".IO.Str,        "abc",      "Str literal .IO";
is "abc".IO.^name,      "IO::Path", ".IO produces an IO::Path";
my $s = "/a/b/c";
is $s.IO.Str,           "/a/b/c",   "Str variable .IO (mut path)";
is $s.IO.parent.Str,    "/a/b",     ".IO chains into path methods";
is $s, "/a/b/c",                    "receiver string is not mutated";

# --- numeric Cool receivers stringify first ---
is 42.IO.Str,           "42",       "Int .IO";
is 3.14.IO.Str,         "3.14",     "Rat .IO";
is True.IO.Str,         "True",     "Bool .IO";

# --- IO::Path type object returns itself (identity) ---
ok IO::Path.IO === IO::Path,                "IO::Path.IO is identity";
ok IO::Path::Unix.IO === IO::Path::Unix,    "IO::Path::Unix.IO is identity";

# --- an IO::Path instance round-trips (handled by the lexical native path) ---
is "/x/y".IO.IO.Str,    "/x/y",     "IO::Path instance .IO preserves path";

# --- null byte in the path string fails ---
dies-ok { "a\0b".IO },                      ".IO with a null byte dies";

# --- Junction receiver autothreads (falls through to the interpreter) ---
my $j = ("a"|"b").IO;
is $j.WHAT.^name,       "Junction", "Junction .IO autothreads to a Junction";
ok so($j.basename eq "a" | "b"),            "autothreaded IO::Path basenames";

# --- $*CWD is inherited by a relative .IO (shared make_io_path_instance) ---
{
    my $*CWD = "/base".IO;
    is "rel".IO.CWD, "/base", "relative .IO inherits \$*CWD";
}

# --- a list/array .IO is not intercepted (interpreter coerces the list) ---
my @a = "x", "y";
ok @a.IO ~~ IO::Path,                       "Array .IO still yields an IO::Path";

# --- chained from a numeric ---
is 100.IO.sibling("s").Str, "s",            "numeric .IO chains into path methods";

# --- gist round-trip ---
is "/foo".IO.gist, '"/foo".IO',             ".IO then gist";
