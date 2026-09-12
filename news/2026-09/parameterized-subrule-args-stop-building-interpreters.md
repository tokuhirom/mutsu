# A parameterized subrule's arguments stop building an interpreter each

Round 17 of [#7576](https://github.com/tokuhirom/mutsu/issues/7576). Instructions
retired on the ticket's 60-row YAMLish document go **1,845,742,337 ->
1,565,835,736 (-15.16%)**, measured with callgrind Ir throughout and the module
precompilation cache warmed first, per round 14's note.

Round 16 handed off a flat profile — `LocalKey::with` 7.63% across a dozen
callers, allocator ~19.6%, `memcpy` 4.20%, no mutsu function above ~1.9% — and
one named item worth ~0.44%. A fresh `callgrind_annotate --tree=both` found
something else entirely, and not by looking at a self-cost line: two callers of
`parsed_subrule_candidates` with the same callee and wildly different per-call
costs. `for_each_atom_candidate` spent 305 instructions per call; the atom path
spent **5,050**, for 11.59% of the whole program. Everything below follows from
that one ratio.

## A parameterized subrule's arguments were evaluated by a whole interpreter

`<element($indent, 0)>` evaluates each argument expression before it can resolve
the rule. `eval_regex_expr_value` has a fast path for a bare `$name` and sends
everything else — including the literal `0` — through
`parse_regex_code_cached`, a freshly built scratch `Interpreter`, and
`eval_block_value`. A scratch interpreter is ~76 heap allocations, five
multi-kilobyte struct moves and a 13k-instruction drop; evaluating the constant
`0` cost about **107,000 instructions**, and this document did it **811** times.

A literal is its own value: nothing in it reads the env, the captures or the
package. `parse_regex_code_cached` wraps the source in `(...)`, so the parse
comes back as `[SetLine(n), Expr(Grouped(Literal(v)))]` — exactly the shape the
compiler turns into one `LoadConst`. `constant_stmt_value` recognizes it and
returns `v`. The list of admitted value kinds is positive rather than an
exclusion, so a new `Value` kind has to be let in deliberately: the one
`Expr::Literal` the compiler does *not* treat as a constant is a code-bearing
regex, which loads as a closure over its enclosing scope.

The bare-`$name` path went with it. It built the whole evaluation env —
`self.env` cloned, then the positional captures, `/`, the named captures and the
in-regex lexicals layered on — in order to read one key out of it, 526 times at
~10,400 instructions each. The guard above it admits only an identifier, which
can collide with none of the capture key shapes, so the two sources that can
actually answer are read directly, in the same precedence.

Together: `Interpreter::new` inclusive **63.9M (3.46%) -> 38.7M**, and
`make_regex_eval_env`, `eval_regex_arg_list` and `eval_regex_expr_value` all
leave the profile.

## …and then the resolution itself was re-run at every position

What that left was `resolve_token_patterns_with_args_in_pkg` at **9.10%** for
**692** calls: binding the arguments in a scratch interpreter, evaluating the
rule body, and running three textual passes over the resulting pattern before
parsing it — once per `<rule($arg)>` probe, at every position.

`PARSED_TOKEN_ARG_CANDIDATES` exists to memoize exactly that, on
`(package, rule name, rendered arguments)` plus the token-registry generation.
It was storing nothing here, because it gated on `pattern_static_modulo_params`:
a syntactic predicate over the pattern text that required every `$` in it to
introduce one of the rule's own parameters. `$<sp>`, `$0` and `$(...)` are not
parameter references at all — they are capture forms the interpolation pass
provably leaves alone — but the predicate called them dynamic, and they appear
in most of YAMLish's parameterized rules (`block`, `map`, `sequence`,
`cuddly-list-entry`, `plain`). Meanwhile it *trusted* the two steps that can
read anything: the evaluation of the rule body, and the evaluation of the
argument expressions.

The predicate is gone. The exclusions are raised **at the reads themselves**
now — round 13's lesson on the sibling `REGEX_SUBPATTERN_PARSE_CACHE`, applied
one layer up. `regex_arg_purity` records a resolution, and the flag goes up
when:

* `interpolate_bound_regex_scalars` substitutes a name that is not one of the
  bound parameters (the value came from the caller's lexical scope, which the
  key does not carry);
* `eval_regex_expr_value` needs the general evaluator for an argument (arbitrary
  code, reads that cannot be attributed);
* a candidate has a parameter default or a `where` clause (binding evaluates
  them against the caller's env);
* a rule body is not a constant;
* the parse of a resolved candidate raises its own ambient-read flag —
  `note_regex_parse_ambient_read` raises both, because this memo stores the
  *parsed* candidates.

Frames nest and the flag OR-restores outward, so an inner impurity is never lost.
`resolve_token_patterns_with_args_in_pkg` **160.3M (9.10%) -> below the
threshold**.

The memo is bounded the way the parse cache is, and for the same reason: its key
carries *rendered arguments*, which are runtime data, so a grammar parameterized
on the text it is parsing would otherwise mint one entry per distinct value.

## Tests

Two new pins in `t/grammar/`, both verified against rakudo 2026.07:

- `grammar-rule-constant-argument-forms.t` — every literal shape (integer,
  negative, both string quote forms, rational, boolean) still arrives at the
  rule with the value it was written with;
- `grammar-rule-arg-memo-outer-lexical.t` — a rule body that splices an *outer*
  lexical is not served from the memo after that lexical changes, and a rule
  that reads only its own parameters keeps matching correctly across repeated
  resolutions, `$<name>=[...]` captures included.

## Method note

Rounds 11-16 each found their item by resolving a flat profile through caller
attribution. Round 17's addition is narrower and mechanical: **when one function
appears under two callers with an order-of-magnitude difference in cost per
call, that ratio is the finding.** `parsed_subrule_candidates` was not on any
"top" list — 0.24% self — and its two caller lines sat next to each other in the
tree output reading 305 and 5,050 instructions per call. Neither number means
anything alone; the gap between them named the memo that was not storing.

The corollary is about what a memo exclusion may be made of. A syntactic
predicate is not merely brittle, as round 13 found — it is also *wrong in both
directions at once*: `pattern_static_modulo_params` refused most of a real
grammar over capture forms that read nothing, and waved through a body
evaluation that could read anything. Raising the exclusion where the read
happens fixed both halves in one change and deleted the predicate.
