# `Parameter.slurpy` / `.positional` were wrong for slurpy parameters, causing a real deadlock in a Cro::HTTP::Router test

`Cro::HTTP`'s own `http-router-named-urls.t` test suite deterministically
hung (`rc=124`) after its 30th assertion, always at the same call:
`abs-link('css', 'x', 'y', 'z')` against a route declared as `get
:name<css>, -> 'css', +a { }` — a literal path segment followed by a bare,
no-sigil slurpy positional parameter (`+a`, Raku's "single-arg-rule"
slurpy).

## Root cause

`Parameter.slurpy` and `Parameter.positional` were computed from mutsu's
internal `SigParam` flags incorrectly for slurpy parameters:

- `SigParam` tracks three *separate* internal booleans for the three slurpy
  shapes: `slurpy` (`*@a`), `double_slurpy` (`**@a`), and `onearg` (`+a`).
  `Parameter.slurpy` only ever read the first of these, so `+a` reported
  `.slurpy == False` (raku: `True`).
- `Parameter.positional` was computed as `!named && !capture`, without
  excluding slurpy params at all, so `*@a`/`**@a`/`*%b` all incorrectly
  reported `.positional == True` (raku: `False` for every slurpy variant).

Verified directly against `raku` for all three slurpy shapes plus a plain
positional, a named, and a capture parameter — mutsu diverged on 4 of the 6
cases before this fix (`+a` `.slurpy`; `*@a`/`**@a`/`*%b` `.positional`).

`Cro::HTTP::Router::LinkGenerator`'s `signature-to-sub` (a general-purpose
Raku helper with no mutsu-specific quirks) relies on `.positional`/`.slurpy`
exactly as raku defines them to detect "this route has a slurpy positional
that can absorb extra URL segments." With mutsu's wrong values, it never
set its internal `$has-slurpy` flag for a `+a` route, so generating a link
with extra positional args hit `die "Extraneous arguments"` deep inside a
closure invoked from `abs-link`. That exception was silently lost rather
than propagating to the test or failing the response Supply, leaving
`test-route-urls`'s `$responses.receive` blocked forever with nothing left
running anywhere in the process — confirmed via `rust-gdb` thread dumps
that all 4 OS threads were parked (the two worker-pool threads were simply
idle, unrelated to the request at all; only the interpreter thread and the
`main()`-joining thread existed, and the interpreter thread sat permanently
in `SharedChannel::receive_result`).

## Fix

`src/value/signature.rs`'s `build_parameter_attrs`: `.slurpy` is now `true`
for any of `slurpy || double_slurpy || onearg`, and `.positional` excludes
that same combined flag. Added `t/parameter-introspection.t` coverage for
all three slurpy shapes (`*@a`, `**@a`, `+a`), verified byte-for-byte
against `raku`.

## Result

`http-router-named-urls.t` (Cro::HTTP::Router's own test suite, run under
the vendored Cro checkout) now passes cleanly and deterministically:
**39/39, rc=0**, no hang, verified 3/3 stable runs. This is independent of
the still-open `todo/deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md`
build-nondeterminism bug — confirmed on the exact same binary that
`http-router.rakutest` (which exercises that unrelated bug) was still
failing with "unknown trait" while `http-router-named-urls.t` passed fully,
proving the two are unrelated.
