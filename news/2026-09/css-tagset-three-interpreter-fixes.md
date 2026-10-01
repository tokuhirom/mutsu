# Three interpreter fixes found by driving CSS::TagSet

Working the `CSS::TagSet` distribution (which none of its three test files could even construct
under mutsu) exposed three general bugs, each now fixed and pinned in `t/`:

- **`%` attribute built from an itemized hash.** `has %.pm` initialised from `${...}` or `my $h = {...}`
  kept the item holder, so `%!pm{$k} = $v` replaced the whole hash instead of storing one element
  (CSS::Module's `has Hash %.property-metadata` lost 128 of 129 properties on the first `extend`).
  `coerce_attr_value_by_sigil` now peels the holder like a `%`-sigil parameter bind.
- **Anonymous `state $` clobbered a method caller's `self`.** The array-share promotion in
  `vm_attr_share.rs` walked saved caller frames using the *callee's* slot layout whenever the caller's
  env held the same key; a method frame mirrors `__ANON_STATE__` (its implicit param), so on the second
  call of `sub f { state $ //= [...] }` slot 0 of the caller -- `self` -- was overwritten.
- **Rule parameters visible to actions.** `token usage($*USAGE) {...}` bound `$*USAGE` only while the
  rule matched; its action, run later in the reduce walk, saw no such variable. The values are now
  recorded on the capture node (like `:my $*x`) and re-installed around the action.
