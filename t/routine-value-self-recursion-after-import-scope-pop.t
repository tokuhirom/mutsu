use Test;

plan 3;

# A captured `&name` reference to a proto/multi export whose import scope has
# since popped. `my (&xrecur) = do { use lib 't/lib'; use
# ProtoRecursionFixture; (&xrecur) }` captures `xrecur` -- a `proto`/`multi`
# pair, so there is no single candidate body to point at -- out of a `do {}`
# block, exactly the shape a selective import uses (`roast/S32-list/skip.t`:
# `my (&plan, &subtest, ...) = do { use Test; (&plan, &subtest, ...) }`,
# done so `Test`'s own exported `&skip` does not shadow the core `skip`
# routine). Once the `do` block's import scope pops, the importing
# package's alias for `xrecur` is correctly removed (an import is lexically
# scoped to its block) -- but `&xrecur` itself must keep working, because
# Raku's real semantics bind a captured `&code` value to the actual routine,
# independent of whether the short name used to capture it stays visible.
#
# 2026-08-18 (#... commit 1237ce8e8): calling `xrecur(...)` after the import
# scope popped used to recurse forever and abort with a Rust stack overflow
# instead of raising a catchable error -- fixed by making
# `call_function_fallback` recognize a dead-end self-referential `Routine`
# value instead of redispatching it. That fix stopped the crash but did NOT
# make the construct actually run: it still died with "Unknown function:
# xrecur", a regression from real Raku (confirmed directly against `raku`:
# this exact construct runs cleanly and returns the dispatched candidate's
# result).
#
# 2026-08-29 (todo/deep/vendor-real-test-module.md, "routine-value
# self-recursion after an import scope pops"): the DANGLING reference itself
# was the real bug. `resolve_code_var` captured `&xrecur` as a LAZY
# `Value::routine_parts(current_package, name)` reference that re-resolves
# "xrecur" BY NAME at call time -- which only worked through the *importing*
# package's alias, exactly the one `pop_import_scope` correctly removes.
# Fixed by materializing the actual multi-candidate bodies BY VALUE at
# capture time instead (mirroring how a multi with no explicit proto was
# already captured), so the resulting Sub keeps working regardless of what
# the registry does to the name afterward. See
# `news/2026-08/begin-selective-import-proto-multi-value-capture.md` and
# `t/begin-selective-import-proto-multi.t` for the fuller BEGIN/list-binding
# matrix this also covers.

my $code = q:to/CODE/;
    my (&xrecur) = do {
        use lib 't/lib';
        use ProtoRecursionFixture;
        (&xrecur)
    };
    say xrecur(1);
    say xrecur("hi");
    CODE

my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);

is $proc.exitcode, 0, 'the captured proto/multi dispatches after its import scope pops';
is $out, "xrecur-int(1)\nxrecur-str(hi)\n",
    'dispatch actually picked the matching multi candidate for each call';
is $err, '', 'no crash / error output on stderr';
