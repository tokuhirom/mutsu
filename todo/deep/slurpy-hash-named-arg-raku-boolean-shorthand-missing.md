# `.raku` on a `Hash` populated from slurpy `*%h` named-arg binding doesn't abbreviate Bool::True pairs

> **Moved from `todo/tickets/` to `todo/deep/` on 2026-08-26**, acting on this file's own
> conclusion. The 2026-08-26 re-measurement below establishes that the signal rakudo consults is
> per-value Scalar-containerness, so matching it requires per-element containers in `HashData` —
> the associative half of [element-itemization-lost-in-scalar-binding.md](element-itemization-lost-in-scalar-binding.md),
> touching every read, write, iteration, `.pairs`/`.Map`/`.kv` path and every `.raku`/`.gist`
> consumer. The file already says "fold this into the element-itemization deep item rather than
> fixing `Hash.raku` in isolation" and names the tempting shortcut as provably wrong. A ticket
> whose own recommendation is "do not fix this as a ticket" does not belong in the oldest-first
> ticket queue, where it can only be picked up and deferred again.

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Pair.rakudoc:61`).

**Re-measured 2026-08-26 against raku v2026.06. The original "root cause
hypothesis" below was directionally right but named the wrong mechanism, and the
real mechanism makes this a deep item, not a ticket-sized one. Read the
"Measured root cause" section, not the hypothesis, before starting.**

## Minimal repro

```raku
sub s(*%h){ say %h.raku };
s :a1:b2;
```
- `raku`: `{:a1, :b2}`
- `mutsu`: `{:a1(Bool::True), :b2(Bool::True)}`

Also reproduces with explicit `=>` syntax at the call site (ruling out a
colon-pair-specific marker):
```raku
sub s(*%h){ say %h.raku }; s a => True;
```
- `raku`: `{:a}` — and `s a => False, b => 1` gives `{:!a, :b(1)}`
- `mutsu`: `{:a(Bool::True)}`

Does **not** reproduce for a plain hash literal/list assignment (both sides
agree, full form):
```raku
my %h = a => True, b => False, c => 1; say %h.raku;
# both raku and mutsu: {:a(Bool::True), :b(Bool::False), :c(1)}
```

Note when writing a repro: naming the sub `s` and calling it as `s :a1:b2;`
(the doc's own form) hits an *unrelated* mutsu parse bug — the call is taken for
the `s///` substitution and dies with "Unsupported regex adverb :a1". Any other
sub name (`foo :a1:b2`) parses fine, so use one.

## Measured root cause (2026-08-26)

The signal rakudo consults is **per value**, and it is Scalar-containerness:

```
$ raku -e 'sub s(*%h){ say %h<a>.VAR.^name }; s(:a);'   # Bool     -- no container
$ raku -e 'my %h = a => True; say %h<a>.VAR.^name;'     # Scalar   -- containerized
```

Rakudo's slurpy-hash binder `nqp::bindkey`s the named arguments into the hash's
storage raw, whereas `=` assignment creates a `Scalar` per value; `Pair.raku`
renders the adverbial shorthand only for a bare `Bool`. It really is per value,
not per hash — assigning into a slurpy-bound hash afterwards mixes both forms in
one `.raku`:

```
$ raku -e 'sub s(*%h){ %h<c> = True; say %h.raku }; s :a;'   # {:a, :c(Bool::True)}
```

and the raw-ness travels with the hash through `.pairs` and `.Map`
(`(:a, :b(1)).Seq`, `Map.new((:a,:b(1)))`) but is lost through an assignment copy
(`my %g = %h` gives `{:a(Bool::True), :b(1)}`) and kept through a bind
(`my %j := %h` gives `{:a, :b(1)}`).

mutsu **already implements exactly this rule for a standalone `Pair`** — it just
has no containerized hash values to apply it to:

```
$ mutsu -e 'say (a => True).raku'                    # :a              -- correct
$ mutsu -e 'my $x = True; say (a => $x).raku'        # :a(Bool::True)  -- correct
```

`raku_repr.rs`'s `ValueView::Pair` arm abbreviates when the value's view is
`Bool` and does not when it is a `ContainerRef`. But mutsu's `Hash` is a plain
`HashMap<String, Value>`: both `*%h` binding (`Value::hash(hash_items)` in
`bind_signature_params`) and `%h = …` assignment store the bare `Bool`, so there
is nothing to discriminate on, and `Hash.raku`'s hash arm accordingly hardcodes
the long form with a comment asserting rakudo always uses it — which the
measurements above show is wrong.

## Why this is bigger than the ticket assumed

Making mutsu match requires hash **values** to be containerized on assignment
and left raw on slurpy binding — i.e. per-element containers in `HashData`. That
is the associative half of the store-side element-itemization work
(`todo/deep/element-itemization-lost-in-scalar-binding.md`, unblocked by
ADR-0013 §7), and it has broad blast radius: every read, write, iteration,
`.pairs`/`.Map`/`.kv` path and every `.raku`/`.gist` consumer.

The tempting shortcut — tagging the `HashData` itself as "bound from named args"
— is a band-aid and is provably wrong: it cannot express the mixed
`{:a, :c(Bool::True)}` case above, and it would have to be dropped on
assignment-copy but kept on bind.

Recommendation: fold this into the element-itemization deep item rather than
fixing `Hash.raku` in isolation. Do **not** "fix" it by making `Hash.raku`
abbreviate every `Bool::True` value — that would regress the hash-literal case,
which mutsu currently gets right.

## Affected files

- `src/builtins/methods_0arg/raku_repr.rs` — the `ValueView::Hash` arm (the
  hardcoded long form, and its now-known-wrong comment)
- `src/runtime/types/binding_signature.rs` — the `*%h` slurpy branch
  (`Value::hash(hash_items)`)
- `src/value/mod.rs` — `HashData`, if per-value containers are added
