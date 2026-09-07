# The `is <Type>` capture-cell exclusion is gone

`CompiledCode::compute_free_vars` used to subtract, from
`needs_cell_unvouched_containers`, every name any `ApplyVarTrait` in the frame
mentioned. Same-named `my` locals share one slot, so that exclusion was **by
name across the whole frame**: one `my %h is BagHash` anywhere in a frame opted
*every* `%h` in it out of the capture cell.

```raku
my %h = a => 1, b => 2, c => 3;
my $f = -> { %h<d> = 4; %h.elems };
sub c1() { my %h = z => 9; $f.() }
say c1();
if False { my %h is BagHash = q => 1 }   # a different, dead declaration
# raku:  4
# mutsu: 2   (before)
```

Delete the last line and mutsu answered 4. With it, the mutating capture in the
first block had neither defence — no vouch (it writes) and no cell — so the
caller's `%h` won.

## Why the exclusion existed

`my %h is BagHash = a => 1, b => 0, c => 2` builds a plain `Hash` at the
declaration store and lets `ApplyVarTrait` coerce it to the QuantHash
afterwards, reading the slot back to find the initial values. A `ContainerRef`
in that slot is not the `Hash` it looks for, so the initialiser was silently
discarded and `%h` came out with one key instead of two. The exclusion was
load-bearing exactly as written: removing it alone failed 39/344 subtests in
`roast/S02-types/baghash.t` and 40/295 in `mixhash.t`.

## The fix: make every `is <Type>` consumer see through the cell

Five sites, all reading or writing the declared container behind the trait:

1. **`exec_apply_var_trait_op`** now goes through `read_var_trait_target` /
   `write_var_trait_target`, which deref on the way in and write *into* the cell
   on the way out. The write helper returns whether it went through a cell; when
   it did, the caller must not also `set_env_with_main_alias`, since the env
   mirror is the same `ContainerRef` and overwriting it with the bare value
   would leave env de-celled while the slot stayed celled.
2. **The declaration store's metadata tagging** (`exec_set_local_op_inner`)
   tags the value *inside* the cell. Tagging the `ContainerRef` was a no-op, so
   the container never learned its declared type.
3. **`coerce_hash_var_value`** reads the current value through the cell, and
   now also consults the container's `declared_type` — where an
   `is BagHash`/`is SetHash`/`is MixHash` records the container's own type —
   in addition to `element_constraint_for`, which reports the *element* type and
   answers `None` for a QuantHash whose `value_type` is empty. Which of the two
   spellings survives on the container depends on whether the declaration store
   or the trait application tagged it last. Without this a whole-container
   re-assignment (`%h = <e e e e e f g>`) fell through to the plain-hash
   initializer and died on the odd element count.
4. **`exec_index_assign_expr_named_op_inner`**'s QuantHash classification
   (`target_is_quanthash`, the `.WHICH`-keying decision, the parameterised
   key-type probe) uses the already-computed `index_target_deref`. A celled
   `BagHash` matched none of the QuantHash arms, so `%h<c> = 0` inside a closure
   was element-type-checked against the *container* type and died with
   "Type check failed for an element of %h; expected BagHash but got Int (0)".
5. **`exec_post_incdec`** classifies and reads through the cell, and its
   in-place writeback locks the cell and mutates the value inside it — a deref'd
   clone would COW-detach on `gc_data_mut` and silently drop the write, which is
   why `%h<k>++` inside a closure did nothing. Its existing `ContainerRef` fast
   path is kept for plain `Hash`/`Array` inners and now steps aside for a
   QuantHash, whose RO check, `original_keys` bookkeeping and SetHash `Bool`
   result all live on the generic path.

## Measured against `raku`, all matching

The repro; a celled `BagHash` element assign, `:exists`, `.elems`, `.keys` and
`.^name`; `++`/`--` through a closure, including the removal a decrement to zero
performs; and a whole-container re-assignment from a word list.

## Testing

`roast/S02-types/{baghash,mixhash,bag,set,mix,sethash}.t` all pass (1676
assertions) — previously 36 subtests failed with the exclusion removed.
`t/typed-container-capture-cell.t` and `t/container-capture-cell-dichotomy.t`
still pass, with the repro added to the latter as its 24th assertion; it passes
unchanged under rakudo.
