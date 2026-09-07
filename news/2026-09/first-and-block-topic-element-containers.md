# `.first`, an associative slice and a block call hand out element containers

Section B of
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` is not an
immutability family at all — it is the opposite failure: rakudo performs these
writes and mutsu silently lost them.

```raku
my %h = a=>1,b=>2; for %h<a b> { $_ = 5 }; %h    # raku {a=>5,b=>5}  mutsu was unchanged
my @a = 1,2,3; @a.first({ $_ = 5 }); @a          # raku [5 2 3]  mutsu was [1 2 3]
my $v = 1; my $b = { $_ = 9 }; $b($v); $v        # raku 9        mutsu was 1
my @a = 1,2,3; my $b = { $_ = 9 }; $b(@a[0]); @a # raku [9 2 3]  mutsu was [1 2 3]
```

Pinned by `t/first-scans-element-containers.t`,
`t/block-call-binds-topic-raw.t` and `t/slice-hands-out-element-containers.t`
(whose five POSITIONAL-slice rows are `todo`-marked — see the last section).

## Two narrowings this needed, both found by the bundled-battery gate

The change as first written regressed four whitelisted battery files, and each
had a different cause.

**`Log::Timeline` (3 files).** The block-call topic alias fired for *every*
block call, and the alias is not free: it installs a cell in the CALLER's env
under the argument's source name and registers an exit writeback. Nested
`Task.log: { ... }` blocks never touch `$_`, and the writeback re-published an
outer task's state over the inner one's — `logging.rakutest` reported the OUTER
task's id for the inner task's end entry. `CompiledCode::writes_topic` (set in
`compute_free_vars` when the body writes `_` by name) now gates it, so a block
that cannot write the topic pays nothing and observes nothing.

**`Text::CSV` (1 file), which parked the positional half.** Handing out the
array's own element containers from a POSITIONAL slice corrupts
`csv(in => $aoa.iterator, out => $fno)`: every row of `$aoa` becomes
`IterationEnd`, caught by the whitelisted `t/90_csv.t` test 503 "AOA parse out".
The associative half (`%h<a b>`) is unaffected and ships. `slice_array_entry`
is therefore not in this change; the rows that need it are `todo`-marked and
the producer is recorded in the ticket's section B.

A process note worth keeping: the first bisect of that failure named the wrong
culprit, because `scripts/battery-testsuite.sh` was running in the background
while the probes ran. Two gates share one workdir and produce false
REGRESSIONs, exactly as CLAUDE.md warns. Every number above was re-measured
with nothing else running, and against a `main` baseline confirming the file
passes there.

## One gap, not three

All three rows turned out to be the same missing thing, and the ticket's stated
root cause was wrong for two of them.

`@a.values` has handed out the array's live `Scalar` cells since ADR-0045
(`Interpreter::array_element_producer`), and everything downstream of a cell
already works: `@a.values.first({ $_ = 5 })`, `@a.values.map({ $_ = 5 })`,
`for @a.values { $_ = 5 }` and `my $b = { $_ = 9 }; $b(@a.values[0])` all wrote
through before this change. So the topic machinery, the `.first` scan and the
block-call binder were all *already* correct — what was missing was a producer
in front of them.

- **A slice** hands out the container's own elements in raku
  (`@a[0..1]` *is* the two `Scalar`s), not copies of their values. The ticket
  proposed carrying the source name and the index list to `for`'s writeback;
  that is not what raku does and would not have covered `.map`/`.grep` over a
  slice or a slice element passed as an argument.
- **`.first`** was recorded as "never reaches the topic marking at all — only the
  two map loops and the grep loop consult `CompiledCode::immutable_topic`". That
  is the *immutability* question (section A). Section B's `.first` row was the
  producer gap: `@a.Seq.first({ $_ = 5 })` already wrote through, `@a.first(...)`
  scanned bare items.
- **A block called with an argument** binds its implicit `$_` **raw**, so the
  argument has to arrive as the caller's container.

## What shipped

`Interpreter::promotable_array_len` / `array_element_cells` /
`array_element_cell_at` / `hash_element_cell_at` (`vm/vm_element_producers.rs`)
factor the gate `array_element_producer` already applied — a real, mutable,
plain array with ordinary storage and at most one dimension; a `Hash` that is not
a `Map` — into one place, so every producer asks the same question:

- **the slice read** (`vm/vm_var_index_ops.rs`) promotes each in-bounds element
  of a mutable source. An index past the end deliberately declines, because
  `@a[0..5]` on a three-element array reads three `Any`s and must not grow the
  array to produce containers for them;
- **`.first`** (`vm/vm_native_first.rs`, and the adverb-carrying
  `dispatch_first`) scans the element containers and decontainerizes its
  *answer*, since `.first` returns the element's value.

The block-call half is two new mechanisms:

- **`Interpreter::pending_call_topic_source`** — the exact sibling of
  `pending_call_topic_bare`, which already answered the same question's other
  half ("the argument has no container at all, so refuse `$_ = ...`"). Set by the
  two value-call opcodes when the sole positional argument names a plain scalar
  lexical, read by `call_compiled_closure_with_topic` before it pushes its frame,
  and cleared by `push_call_frame` — that lifecycle is what keeps a native
  `.map`/`.first` loop's own `pending_call_arg_sources` from being mistaken for
  the block's. The topic is then bound through the same shared `ContainerRef`
  cell recipe `binding_signature.rs` uses for an `is rw` parameter aliasing a
  plain scalar caller variable, so writes go through the cell rather than being
  snapshotted at frame exit.
- **`OpCode::IndexArgRef`** — ADR-0067's subscript-ARGUMENT producer, the twin of
  `IndexInvocantRef` one position over. The named-callee spelling `g(@a[0])`
  already worked through `CallFunc`'s copy-in/copy-out temp protocol; the three
  nameless-callee spellings had no producer at all, so a plain `Index` had
  already read the element's value by the time the call ran. The op replaces the
  argument's trailing `Index` and is gated at run time on the real callee — the
  same gate `MarkRwArgRefContextCallee` uses, one stack slot deeper, plus the
  bare-block topic rule a signature cannot express.

That second one also closes the headline of
`todo/tickets/subscript-argument-container-producer.md`, exactly as that ticket
predicted it would: `S.new.take(@a[0])`, `my $r = &g; $r(@a[0])` and
`my $b = -> $x is rw { $x = 9 }; $b(@a[0])` all bind the element now instead of
dying with "expects a writable container".

## Three consumers the promotion surfaced, all fixed with it

Making a slice hand out containers is a change to what an element *is*, and the
local suite found exactly three places that were reading a slice's items
structurally:

- **`@a[0,1]»++`** desugars to "read the OLD slice, mutate it in place, discard
  the NEW" — and the OLD read is now the same containers the mutation writes
  through, so it reported the new values. It gets an explicit `DecontListElems`
  snapshot, which is what "the post-increment result" means.
- **A defaulted HOLE.** `@a[3]:delete` leaves a `Package("Any")` marker that
  `resolve_array_entry` reads as the container's `is default(...)` value;
  promoting it would hand back the marker instead. The slice declines to promote
  a hole that reads as a default.
- **The list-destructuring staging temp.** `my ($g, %rest) = f(...)` compiles the
  slurpy target as `@__destructure_tmp__[1..*]`, and that temp is not a user
  Array — it *is* the RHS list, every target reads a VALUE out of it (ADR-0040
  slice 2 already suppresses element itemization for it for the same reason). It
  is excluded from the shared promotion gate.

The third one has a second face worth recording: the obvious alternative — teach
the hash initializer to decontainerize its items — does not work, because mutsu
spells "a `$`-sourced itemized hash" and "a hash in an element cell" the same
way. `my $hi = %h; my %c = ($hi,)` must die "Odd number", and unwrapping the cell
loses the itemization that makes it die. That ambiguity is recorded in
`todo/deep/containerref-holding-a-hash-is-indistinguishable-from-itemization.md`.

## What is left in section B

Producer 1 (`@a.list.map({ $_ = 7 })`) is untouched: its route runs through
`SeqSource::MapGrep`, whose own blocker is recorded in
`todo/deep/deferred-map-callback-runs-in-the-consuming-frames-env.md` and
ADR-0058 §9.
