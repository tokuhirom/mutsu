use Test;

# `.absolute` / `.relative` on an IO::Path derive a string from the path and the
# cwd ($*CWD / the instance cwd / the process cwd). The cwd is read through
# `&self` helpers (resolve_path/get_cwd_path/apply_chroot) that are purely
# lexical (no filesystem access), so the bytecode VM dispatches them natively,
# sharing the single impl the interpreter's `native_io_path` also uses.

plan 14;

{
    my $*CWD = "/base/dir".IO;

    # --- .absolute ---
    is "rel/x".IO.absolute,        "/base/dir/rel/x", "absolute of relative uses \$*CWD";
    is "/abs/y".IO.absolute,       "/abs/y",          "absolute of already-absolute is itself";
    is "rel/x".IO.absolute("/o"),  "/o/rel/x",        "absolute with an explicit base";
    is "/abs/y".IO.absolute("/o"), "/abs/y",          "absolute base ignored when already absolute";

    # --- .relative ---
    is "/base/dir/sub/f".IO.relative,          "sub/f", "relative strips \$*CWD prefix";
    is "/base/dir/sub/f".IO.relative("/base"), "dir/sub/f", "relative with an explicit base";
    is "/elsewhere/g".IO.relative,             "/elsewhere/g", "relative of a non-descendant is unchanged";

    # --- variable receiver (mut dispatch path) ---
    my $p = "data/file".IO;
    is $p.absolute,            "/base/dir/data/file", "absolute (variable receiver)";
    is $p.relative("/base"),   "dir/data/file",       "relative (variable receiver)";
    is $p.Str,                 "data/file",           "receiver not mutated by absolute/relative";

    # --- chaining ---
    is "a/b/c".IO.parent.absolute, "/base/dir/a/b", "parent then absolute";
}

# --- the instance's own cwd attribute wins over the process cwd ---
my $rooted = IO::Path.new("rel", :CWD("/inst"));
is $rooted.absolute, "/inst/rel", "instance cwd attribute drives absolute";

# --- absolute/relative round-trip ---
{
    my $*CWD = "/home/u".IO;
    my $abs = "proj/main".IO.absolute;
    is $abs, "/home/u/proj/main",                "round-trip: absolute";
    is $abs.IO.relative, "proj/main",            "round-trip: relative recovers the path";
}
