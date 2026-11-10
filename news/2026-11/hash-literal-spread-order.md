# `{ %h, k => v }` keeps source order, so a later pair overrides the spread hash

The anonymous-hash composer collected its literal pairs and its `%h` spreads in two lists and
emitted the pairs first, so `{ %input, remove => 'ab' }` let `%input`'s old `remove` win
(LIVR `t/10-livr-2.5.t`). Pairs seen before a spread are now flushed ahead of it.
