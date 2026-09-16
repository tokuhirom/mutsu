# A `where` clause on a slurpy hash parameter was never checked, and two `nqp::` string ops made a hand-rolled scanner quadratic

Three interpreter bugs, found by making `App::ShowPath`'s test suite pass
(`t/01-basic.rakutest`, `red` → `green`). The distribution itself never uses
any of the three constructs directly — they surfaced two dependencies deep,
in `License::SPDX` and `JSON::Fast`.

## 1. `multi method new(*%v where { not $_.keys })` matched every call

`License::SPDX` (an `App::ShowPath` dependency via `META6` → `Test::META`)
declares:

```raku
class License::SPDX does JSON::Class {
    multi method new(*%v where { not $_.keys }) {
        self.from-json(%?RESOURCES<data/licenses.json>.slurp);
    }
}
```

The intent is a zero-args-only constructor: `License::SPDX.new` loads the
bundled SPDX license list, and every other call (in particular
`.from-json(...)`, whose generic object-construction path calls
`type.new(|%args)` with real field data) falls through to the ordinary
`Mu`-provided constructor. mutsu's binder and its separate multi-candidate
trial-matcher both had a `!pd.slurpy` filter on the code path that evaluates
a named parameter's `where` clause — a plain named param (`:$x where {...}`)
got the check, a *slurpy hash* (`*%v where {...}`) did not, in either place.
So the zero-args candidate matched **every** constructor call regardless of
arguments:

```raku
class Foo {
    has $.x;
    multi method new(*%v where { not $_.keys }) {
        say "special"; self.bless(x => 'default');
    }
}
Foo.new(x => 1);  # rakudo: silently uses the default ctor, $.x == 1
                  # mutsu (before): prints "special", $.x == 'default'
```

For `License::SPDX`, that meant `.from-json($real_json)` kept re-triggering
the zero-args branch, which re-parses the class's own bundled 332KB resource
file — recursively, since that re-parse itself calls `.new` again on each
nested `License::SPDX::License` object. What looked like a hang was this
recursion, not an infinite loop in any single function.

Fixed in both places: `bind_function_args_values_inner`'s `is_hash_slurpy`
arm (`src/runtime/types/binding_signature.rs`) and the trial-matcher's
candidate-selection closure (`src/runtime/types/args_matching.rs`) now
evaluate a slurpy hash's own `where` clause against the collected hash,
exactly as the existing `|c where {...}` capture-parameter arm already did
next to each of them. A candidate whose guard fails is rejected during
dispatch (falls through to another candidate) and, if it is somehow the only
candidate, raises the ordinary `X::TypeCheck::Binding::Parameter` a failed
`where` clause raises anywhere else — not a silent match.

Pinned in `t/routines/dispatch/multi-where-slurpy-hash.t`.

## 2. Four `nqp::` string/text ops re-scanned their whole string argument on every call

`JSON::Fast`'s hand-rolled parser (used by `JSON::Class`, hence
`License::SPDX`) calls `nqp::iscclass`, `nqp::findcclass`/`findnotcclass`,
`nqp::eqat`, `nqp::index`/`rindex` and `nqp::substr` directly, once per
character or per token, always against the *same* full document string with
an advancing position. Each of `runtime/nqp_ops_text.rs` and
`runtime/nqp_ops_str.rs`'s implementations collected `args[i]` into a fresh
`Vec<char>` (or, for `substr`, walked `char_indices()` from the start) on
every single call — turning O(n) parsing work into O(n) work repeated O(n)
times. A 332KB JSON document (`License::SPDX`'s bundled license list) made
this look indistinguishable from an actual hang.

New shared module `runtime/nqp_char_cache.rs` memoizes a string argument's
`Vec<char>` across consecutive calls, keyed by the underlying `Arc<String>`'s
own pointer identity (with the cache holding its own `Arc` clone, so a
dropped-and-reallocated `Arc<String>` can never alias a stale entry by
pointer reuse). `iscclass`/`findcclass`/`findnotcclass`/`substr` (text ops,
`args[1]`/`args[0]`) and `index`/`rindex` (string ops, the haystack in
`args[0]`) now go through it instead of re-collecting.

Pinned in `t/vm/nqp-text-scan-cache.t` (correctness across repeated calls at
different positions, plus a bound on how long a 20,000-character scan may
take).

## 3. A never-declared `@*`/`%*` dynamic variable auto-vivified instead of reading undefined

`Test::META`'s own `meta-candidates()` does `@*META-CANDIDATES // <META6.json
META.info>` — `@*META-CANDIDATES` is never declared anywhere in the file, so
in real Raku the read comes back as an undefined `Failure`
(`X::Dynamic::NotFound`), and `//` falls through to the literal list.
`GetArrayVar`/`GetHashVar`'s fallback (`src/vm/vm_exec_dispatch.rs`) instead
auto-vivified a **defined** empty `Array`/`Hash` for any never-found
`@name`/`%name`, `*`-twigil or not — so `//` never saw an undefined value,
and `meta-candidates()` always returned `()`, which meant `App::ShowPath`'s
`meta-ok` test could never find its own `META6.json`.

The fallback now distinguishes by twigil: a name spelled `@*.../%*...` that
matches no store anywhere falls back to `Value::NIL` (matching the identical
scalar fallback a few lines away in the same function, and real Raku's
`Failure`), while a plain undeclared `@name`/`%name` keeps mutsu's existing
lenient auto-vivification.

Pinned in `t/vm/scope/dynamic-array-hash-var-not-found.t`.

## Result

`App::ShowPath` goes from `red` (died loading `META6` at all) to `green`
(2/2 files, matching rakudo). `META6`/`Test::META`/`License::SPDX` sit behind
`meta-ok`, the single most common assertion in the whole ecosystem's own test
suites, so this reaches far beyond one distribution: roughly 55 other
ecosystem records currently carry a stale `first_failure` naming the
`json-skip-null` trait (a separate, already-merged fix — [re-exported
routines reaching the importer](https://github.com/tokuhirom/mutsu/issues/8121))
that masked these three bugs behind it. Two spot-checked here as evidence
that the fix generalizes, `Heap` and `Die`, each move their `meta-ok` file
from `die`/`regression` to `pass`/`parity` with no regressions elsewhere. The
rest are left for the corpus sweep's own nightly run rather than re-measured
by hand in this PR.
