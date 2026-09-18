use Test;

# `.absolute` / `.relative` on an IO::Path derive a string from the path and the
# cwd ($*CWD / the instance cwd / the process cwd). The cwd is read through
# `&self` helpers (resolve_path/get_cwd_path/apply_chroot) that are purely
# lexical (no filesystem access), so the bytecode VM dispatches them natively,
# sharing the single impl the interpreter's `native_io_path` also uses.

plan 16;

{
    my $*CWD = "/base/dir".IO;

    # --- .absolute ---
    is "rel/x".IO.absolute,        "/base/dir/rel/x", "absolute of relative uses \$*CWD";
    is "/abs/y".IO.absolute,       "/abs/y",          "absolute of already-absolute is itself";
    is "rel/x".IO.absolute("/o"),  "/o/rel/x",        "absolute with an explicit base";
    is "/abs/y".IO.absolute("/o"), "/abs/y",          "absolute base ignored when already absolute";
    # `.absolute`'s `$base` is POSITIONAL-only (`multi method absolute(IO::Path:D:
    # $base --> Str)`); a `base => ...`/`:base(...)` NAMED argument does not bind
    # to it at all, so rakudo falls back to the zero-arg `$*CWD` candidate rather
    # than using the named value (verified against rakudo 2026.07). mutsu used to
    # stringify the stray named argument's `Pair` itself (`args.first()` picked up
    # the Pair, and a Pair's `.Str` is `"key\tvalue"`), producing a `"base\t..."`
    # prefix instead of ignoring it (WebDriver2 0.1.12, ecosystem sweep — its
    # `WebDriver2::SUT::Tree::URL.new` calls exactly `.absolute: base => $cdir`).
    is "rel/x".IO.absolute(base => "/o"), "/base/dir/rel/x",
        "a NAMED base=> argument does not bind the positional \$base; falls back to \$*CWD";
    is "rel/x".IO.absolute(:base("/o")), "/base/dir/rel/x",
        "a NAMED :base(...) argument does not bind the positional \$base; falls back to \$*CWD";

    # --- .relative ---
    is "/base/dir/sub/f".IO.relative,          "sub/f", "relative strips \$*CWD prefix";
    is "/base/dir/sub/f".IO.relative("/base"), "dir/sub/f", "relative with an explicit base";
    is "/elsewhere/g".IO.relative,             "../../elsewhere/g", "relative of a non-descendant walks up with ..";

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
