# A regex literal does not capture its defining scope

A Raku regex is a closure over the scope it was written in. mutsu represents a
regex value as a bare pattern string (`ValueRepr::Regex(Arc<String>)`, or
`RegexWithAdverbs` when the literal carries adverbs), so code embedded in the
pattern — `{ ... }` blocks, `<?{ ... }>` assertions, `:my`/`:let` initializers —
resolves its free variables against whatever env exists at *match* time. A regex
built in one frame and matched from another silently loses them.

Minimal repro (`raku` prints `idx=1 args=99`; mutsu prints `NO MATCH`, because
the assertion reads an empty `@handlers`):

```raku
use MONKEY-SEE-NO-EVAL;
class RouteSet {
    has $!matcher;
    method generate() {
        my @handlers = "aa", "bb";
        use MONKEY-SEE-NO-EVAL;
        $!matcher = EVAL 'regex { ^ :my $cap; greet { $cap = 99 } <?{ my $han = @handlers[1]; $han.chars == 2 }> { make (1, $cap) } $ }';
    }
    method route(Str $path) {
        with $path ~~ $!matcher { my ($i, $a) = .ast; "idx=$i args=$a" } else { "NO MATCH" }
    }
}
my $rs = RouteSet.new; $rs.generate(); say $rs.route("greet");
```

Pinned (as `todo`) by `t/regex-my-var-closure-capture.t`, assertion 4.

## Why it blocks real code

This is the last blocker for `Cro::HTTP` serving a request whose route has
captured segments. `Cro::HTTP::Router::RouteSet!generate-route-matcher` builds
its path matcher with `EVAL 'regex { ... }'` over a local `my @handlers`, and
`transformer` matches it much later from a `supply` block; the per-route bind
check `<?{ my $han = @handlers[$i]; $han.signature.ACCEPTS($cap) ... }>` then
reads an empty array, so every parameterised route 404s. Literal routes
(`get -> { }`, no bind check) already work — see
`news/2026-08/cro-http-request-round-trip.md` for the five fixes that got the
round-trip that far.

## What was tried, and why it was reverted

A working prototype existed and made the Cro round-trip pass end to end:

- `OpCode::LoadRegexCaptured`, emitted for a regex literal whose pattern
  embeds code (`{`, `:my `, `:let `). Its handler scanned the pattern text for
  sigiled names, resolved each against the loading frame's locals then env, and
  attached the snapshot to the value.
- The snapshot rode in a new `RegexAdverbs::captured` field; a thread-local
  stack (`REGEX_CAPTURED_SCOPES`) was armed for the duration of a match and
  consulted by the variable-read paths (`GetGlobal`, `GetArrayVar`,
  `GetHashVar`, the upvalue fallback, `VarInterp`) **only after every live
  store missed**, so nothing that already resolved could change.

It was reverted because attaching the snapshot required turning a plain
`ValueRepr::Regex` into a `RegexWithAdverbs`, and ~122 sites match
`ValueView::Regex(_)` specifically. Grammar/token registration is one of them:
with the conversion in place, `grammar S { token TOP { 'q' { @log.push('hit') } } }`
stopped running its code block at all (`t/regex-inline-code-block.t` 10,
`t/regex-ltm-declarative-prefix.t` 1-2, `t/grammar-method-subrule.t` 3-4).
Patching only the sites the tests caught is exactly the fragile approach the
working agreements warn against.

## What the real fix needs

The value has to carry the captured scope *without* changing how existing
consumers see a regex. Two candidates:

1. A third repr variant — `RegexCaptured` — carrying `(Arc<String>, Arc<HashMap<String, Value>>)`
   whose `peek`/`view` yields `ValueView::Regex(&pattern)`, so all 122 consumers
   keep working verbatim and only the arming site probes the repr. The NaN-box
   packs one `Arc` per `Kind`, so this needs a payload struct plus the usual
   `Kind`/encode/decode/peek/serde wiring.
2. Audit every `ValueView::Regex(_)` consumer and give them a variant-agnostic
   accessor (`Value::regex_pattern()`), then let the conversion stand. More
   invasive but removes the two-variant split that caused this.

Either way, the capture itself should evolve past the prototype: it snapshots
values, so a later rebinding in the defining scope is invisible. Capturing
shared cells (as closures do) is the sound version — the same argument as the
"cell route is the gain, by-value is the risky one" rule in CLAUDE.md.
