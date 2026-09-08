# A tail bare block's `LEAVE` runs again

`Cro::HTTP`'s `t/http-middleware.rakutest` failed under the vendored `Test`
module — one of the four bundled-library regressions in
[#7555](https://github.com/tokuhirom/mutsu/issues/7555) — with the third
subtest's conditional middleware apparently never installed: requests that
should have come back `403` returned the application's own page, and the
caching subtest saw its counter advance where a cached body was expected.

The middleware was installed. The **previous** subtest's server was still
listening.

## What was wrong

Each of that file's blocks ends the same way:

```raku
{
    my Cro::Service $service = Cro::HTTP::Server.new(…);
    $service.start;
    …
    LEAVE $service.stop();
}
```

That block is the **last statement** of the routine body it sits in, so the
compiler inlines it: a tail block is the body's implicit return value, and
inlining it is how that value reaches the caller. Inlining, however, drops the
block's phasers — the `LEAVE` was compiled away entirely, `$service.stop()`
never ran, and every later request on that port was answered by the pipeline of
a server that was supposed to be gone.

The mainline compiler (`compiler/mod.rs`) already knew this: its tail-block site
routes a block carrying `ENTER`/`LEAVE`/`KEEP`/`UNDO`/`PRE`/`POST` through a
real `BlockScope` instead of inlining it. The three tail-block sites in
`compiler/helpers_sub_body.rs` — one in `compile_routine_body_stmts`, two in
`compile_closure_body_with_routine_flag` — did not, so the bug applied to every
named sub, method, anonymous closure and block argument:

```raku
sub st(&b) { b() }
st {
    { LEAVE say 'a'; }
    { LEAVE say 'b'; }   # tail block: never printed
}
```

Only `do { … }` was unaffected, because it compiles through the expression path
that already had the check.

## The fix

The three sites now make the same check the mainline does and hand a
phaser-carrying tail block to `compile_phaser_block_scope` with
`PhaserBlockResult::Push`, so the block's value still becomes the body's
implicit return while its phasers run. Blocks with no phasers keep the inline
path unchanged.

`Cro::HTTP`'s `t/http-middleware.rakutest` now passes **24/24 under the vendored
`Test`** (`MUTSU_REAL_TEST=1`); before this it died after 31 assertions with
`X::Cro::HTTP::Error::Client`. Pinned by `t/leave-phaser-in-tail-block.t`,
which covers a named sub, a method, an anonymous closure and a block argument,
and checks that the tail block still supplies the body's value.
