# Cro: a route with path-segment parameters still hangs

## Where the Cro campaign stands (updated 2026-08-01)

**Cro now serves a real HTTP request under mutsu.** `Cro::HTTP::Server` with
`route { get -> { content 'text/plain', 'Hello from mutsu' } }` answers `curl`
with a complete `200 OK` response, against pristine upstream Cro sources. Two
general interpreter fixes got it there:

- `news/2026-08/nested-type-short-name-owner-scope.md` — a class's nested type
  keeps its short name after another module registers the same name, so
  `Cro::HTTP::Header.parse` reaches its class-body `my grammar Header` again with
  `Cro::HTTP::Router` loaded. This unblocked header parsing.
- `news/2026-08/regex-my-lexical-in-make-block.md` — a regex `:my` lexical is
  visible to a `make`-bearing code block, so the router's `EVAL`ed path matcher
  reduces with its `$cap` Capture set. This unblocked route dispatch.

What still fails: a route whose signature takes **path segments**.

```raku
route {
    get -> { content 'text/plain', 'ok' }                    # works
    get -> 'greet', $name { content 'text/plain', "hi $name" }  # hangs
}
```

`GET /greet/world` never produces a response (curl times out with 0 bytes).
`GET /` on the same server still answers correctly, so the server, the parser
and the serializer are all fine — the failure is specific to a route that binds
segments.

The signature-`ACCEPTS`-on-a-Capture half of this is now fixed
(`news/2026-08/signature-accepts-literal-param.md`: `Signature.ACCEPTS` existed
only as a smartmatch, and a reflected signature ignored its literal parameters).
The route still does not match, and there is a **minimal standalone repro**.

## Minimal repro: an array `:my` behind a `^` anchor loses its elements

Instrumenting the router shows `$request.path ~~ $!path-matcher` returning no
match for `/greet/world`. Bisected all the way down (`tmp/rxanchor2.p6`), the
trigger is **a leading `^` in front of the declarators**:

```raku
class Req { method segs() { <a b c> } }
my $*R = Req.new;

say ("x" ~~ /   :my @s = $*R.segs; 'x' { make @s.elems } /).ast;  # raku 3  mutsu 3
say ("x" ~~ / ^ :my @s = $*R.segs; 'x' { make @s.elems } /).ast;  # raku 3  mutsu 0
say ("x" ~~ / ^ :my $r = $*R; :my @s = $r.segs; 'x' { make @s.elems } /).ast;
                                                                  # raku 3  mutsu 1
```

An inline `{ … }` block sees the same wrong count, so this is *not* the
reduce-time replay — it is the declaration itself.

**Why the anchor matters.** `parse_regex_declarative_prefix` scans from the very
front of the pattern for `:name`; a leading `^` makes the loop break immediately,
so nothing is hoisted. The `:my` then goes down the *other* path — the
`RegexAtom::VarDecl` atom in `regex_match_capture.rs`, which evaluates the
declaration in a scratch `Interpreter` and harvests the env diff into
`RegexCaptures::regex_vars`. That path mishandles an `@`-sigil declaration whose
RHS is a method call: the elements are lost (0) or collapsed to one.

Cro's generated matcher is exactly the anchored shape
(`regex { ^ :my $req = …; :my @segs = $req.path-segments; :my $cap; [ … ] $ }`),
so `Capture.new(:list(@segs), …)` is built with the wrong number of positionals,
the route's bind check rejects it, and no handler runs.

Two things to decide when fixing: whether the two paths should be unified (the
declarative-prefix hoist ought to look past leading anchors/adverbs), and,
independently, why the `VarDecl` scratch harvest loses an array's elements —
mutsu's dual store is the obvious suspect (`my @s = …` may land in a local slot
rather than the scratch `env` the harvest reads).

## Repro

Scratch clones live under `tmp/cro-work/` with the `-I` list in
`tmp/cro-work/inc-paths.txt`. Clone `croservices/cro-{core,http,tls}`,
`jnthn/raku-log-timeline`, `retupmoca/P6-JSON-JWT`, `japhb/CBOR-Simple`,
`japhb/TinyFloats`, `jnthn/p6-io-socket-async-ssl`; everything else Cro depends
on is already a bundled battery.

```
timeout 200 target/debug/mutsu $(cat tmp/cro-work/inc-paths.txt) tmp/cro-clean.p6
```

(zsh note: expand `inc-paths.txt` inline with `$(cat …)`; a `$VAR` expansion is
not word-split and collapses every `-I` into one argument.)

## Also still open: a module-global short name beats an inner-scope enum value

`Cro::HTTP::RequestParser.transformer` declares, inside its `supply` block,
`my enum Expecting <RequestLine Header Body>`, and does `$expecting = Header`.
`RequestLine` resolves to the enum value correctly, but `Header` resolves to
`Cro::HTTP::Router::Header` — the *role* — because registering that role bound
the bare short name `Header` in the ONE global env, and that binding wins over an
enum value declared in an inner lexical scope.

It happens not to break a request today (`when Header` compares the same wrong
value on both sides, so the header branch is still taken, and a bodyless GET
never evaluates `when Body`), but it is wrong: `$expecting == Body` on a type
object, and the `Body` branch can never match. **A request with a body is
therefore a likely next failure.**

The owner-package-chain probe that landed covers the case where the *owner class*
is asking; it cannot cover a same-named lexical declaration (an enum value, a
`my` type) in an unrelated scope, because that declaration is not reachable from
any owner chain. The real fix is to stop registering short-name aliases in the
global env across module boundaries and keep them per-module, the way the
parser's scan does — the same architecture gap as `package_chain_var_fallback`
(#5658) but for TYPE names.

## Debugging techniques that worked (reuse these)

- `tmp/cro-work` holds plain `git clone`s, not vendored sources — add
  `note "DBG: …"` lines directly to them to bisect the pipeline stage
  (TCP accept -> conn Supply emit -> RequestParser packet -> req-line ->
  header-line -> emit request -> router -> routing outcome -> handler.invoke ->
  ResponseSerializer). `git -C tmp/cro-work/cro-http checkout -- .` reverts them.
- Printing the router's `($handler-idx, $args) = .ast` is what exposed the
  `$args = Any` that should have been a `Capture` — a one-line `note` on the
  value a `make` produced, rather than guessing at the regex engine.
- Drive the client with `run 'curl', '-sS', '-m', '20', …` from the same script
  so the server process terminates on its own.
- `rust-gdb -batch` breakpoints beat `eprintln!` here: breaking on
  `parse_regex_code_cached` and printing its `code` argument showed which regex
  code blocks actually ran, and a backtrace from `eval_regex_inline_code` showed
  the pattern with the `:my` already stripped out — which *was* the bug.
