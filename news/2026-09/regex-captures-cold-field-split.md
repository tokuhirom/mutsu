# The regex capture accumulator was 336 bytes, and every match candidate paid for it

`RegexCaptures` is the accumulator the regex engine builds for **one match
candidate**. A grammar parse constructs, moves, clones and drops millions of
them — 2.59 M over the 60-row YAMLish document
[#7576](https://github.com/tokuhirom/mutsu/issues/7576) has been profiled
against since round 10. It carried sixteen fields inline, including three
`HashMap`s, and came to 336 bytes, so every candidate paid a 336-byte `memcpy`
and three empty-map drops to carry state almost none of them had.

Instructions retired on that document go **2,178,652,179 -> 2,010,535,421
(-7.72%)**. Attribution is callgrind Ir throughout (deterministic,
load-independent), as in rounds 10-14.

## (a) The cold fields move behind one boxed payload (-6.75%)

This is the ADR-0016 P2 `CapNode`/`CapChildren` split applied one level up, to
the accumulator instead of the stored node. Eight fields are written by a
minority of patterns — `:my` declarators (`regex_vars`), capture aliases
(`capture_alias_map`), `%<name>=` hash captures (`hash_captures`), the
pcre2/`:P5` slot axis (`positional_slots`), a protoregex `:sym<>` win (`sym`),
an aliased rule name (`action_name`), and the two engine-entry links (`target`,
`outer_backref`) — and now live in a `RareCaps` payload behind one
`Option<Box<_>>`. `RegexCaptures` is **336 -> 128 bytes**, pinned by
`regex_captures_size_guard`.

Call sites reach the fields through accessors: the read-only ones hand back a
shared empty value when no payload was ever allocated, the `_mut` ones
materialize it. Two properties matter more than the size number and are pinned
by their own tests:

- an accumulator that takes no cold-field write **never allocates** the
  payload, and reading one back does not allocate one to look at (so
  `caps.regex_vars().is_empty()` on a plain candidate is a null check);
- a payload that is drained or cleared again is **dropped**, so an accumulator
  that briefly held a `:my` variable does not make every later clone copy an
  empty payload. That is what `prune_rare` is for, and why the setters take an
  `Option` rather than being paired with separate clear methods.

The merge paths were the subtle part. `dst.regex_vars_mut().extend(src)` would
allocate a payload on `dst` even when `src` is empty — which is the common case
— so the merges go through `extend_regex_vars` / `extend_capture_alias_map` /
`merge_hash_captures`, which look at the source first and return without
touching `dst` when there is nothing to move. Likewise `delta.X_mut().drain()`
grew a payload just to drain it empty; those are `take_X()` now.

Effect on the profile: `memcpy` **154,718,384 (7.10%) -> 77,609,917 (3.82%)**,
`RawTable::drop` 54.1 M -> 34.9 M, `RawTable::clone` 34.0 M -> 28.5 M,
`regex_walk_ends_in_pkg` 37.3 M -> 28.5 M.

## (b) …and the in-regex lexicals are shared, not copied per atom (-1.04%)

Round 14 named this as the item behind item 4: every inline sub-pattern (a
group, an alternative, a lookaround body) publishes the `:my`/`:let` lexicals
in scope to the store it is about to build, and `InlineVarsSeed::arm` did it by
**cloning the whole map**, once per atom match, with
`take_inline_regex_vars_seed` cloning it back out again. YAMLish computes its
block indent in a `{ … }` inside a `<?before …>`, so its parse is exactly the
shape that pays.

`RareCaps::regex_vars` is an `Option<Arc<RegexVarMap>>` now; arming and seeding
are refcount bumps, and the copy is paid only by a level that actually writes a
lexical, through `Arc::make_mut`. `take_inline_regex_vars_seed` leaves the
profile entirely (4.5 M -> not listed); `arm_inline_vars_seed` goes 24.3 M ->
20.7 M.

**A measured wrong turn worth recording:** the first version of this made the
field a plain `Arc<RegexVarMap>`, and it read as a **0.7% regression**
(2,031,713,138 -> 2,045,936,861) with `malloc` and `free` both up. `Arc<T>` has
no empty representation, so `Arc::default()` *allocates* — and `RareCaps`
constructs one on every payload materialization, turning an allocation-free
`HashMap::default()` into a heap allocation. The `Option` wrapper is not
defensive style here; it is the difference between a win and a regression.
Wrapping a rarely-populated collection in `Arc` for sharing is only free if the
absent case stays absent.

## Method note, extending rounds 10-14

Round 14's lesson was procedural: warm the module precompilation cache before
taking a profile. Round 15's is about **where a size win actually lands**. The
336 bytes were never visible under a function's own name — they were `memcpy`,
`_int_malloc` and `RawTable::drop`, exactly the "irreducible allocator tail"
that rounds 11-14 each had to look past. The tell was a struct-size question
(`size_of::<RegexCaptures>()`) rather than a profile line, which is why round
14 could only name the item after it measured the type instead of the run.
Both new guard tests exist so the next field added to the accumulator has to
answer that question at compile time.

## What is left

The profile stays flat: `LocalKey::with` 7.3% across a dozen callers, the
allocator ~19%, `memcpy` 3.9%, and the largest single mutsu function is
`regex_match_atom_all_with_capture_in_pkg_inner` at 1.85%. Two items from
round 14's list are untouched: `resolve_parsed_token_candidates_in_pkg` still
interns its *package* name on every probe (~0.7%), which wants a `Symbol`
threaded down rather than another memo; and the remaining 128 bytes are
dominated by `named` (a 48-byte `HashMap` inline) plus two `Option<usize>`
span-marker fields that could be `u32`-with-sentinel. Neither has a measured
size yet — re-run the caller attribution rather than carrying this paragraph
forward as a conclusion.
