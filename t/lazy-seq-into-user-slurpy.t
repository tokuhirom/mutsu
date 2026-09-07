use Test;

# A lazy `Seq` passed to a USER-defined slurpy must be flattened into it, the
# same way an eager one is. ADR-0058 made `.map` (steps 2/3a/3c) and `.grep`
# (step 3b) answer a not-yet-run `Seq`; the `*@a` binder reads its elements
# through pure code, which cannot pull, so it collected ADR-0034's empty seed
# and the caller's arguments silently vanished.
#
# Step 3b added the reification to the `+@a` arm and left `*@a` to the
# still-eager `.grep` — so deferring `grep` turned `*@a` from 3 elements to 0.
# The discriminator is LAZINESS, not slurpiness: every eager shape below was
# always correct, and a non-slurpy `@a` receives the Seq intact.

plan 12;

# --- the deferred map/grep Seqs (the bug) ---

sub count(*@a) { @a.elems }
sub items(*@a) { @a }

is count((1..3).grep(* > 0)), 3, '.grep into *@a flattens (regression from ADR-0058 step 3b)';
is items((1..3).map(* + 0)).raku, '[1, 2, 3]', '.map into *@a flattens';
is count((1..3).grep(* > 1)), 2, '.grep into *@a keeps the FILTERED count, not the source count';
is count(((1..3).map(* + 0))), 3, 'a parenthesized .map argument flattens too';

# --- eager controls: these were never broken and must stay correct ---

is count((1, 2, 3)), 3, 'control: a literal list still flattens';
is count((1, 2, 3).Seq), 3, 'control: an EAGER Seq still flattens';
is count((1, 2, 3).List), 3, 'control: a List still flattens';
is count(1 .. 3), 3, 'control: a Range still flattens';
is count(1, 2, 3), 3, 'control: three separate arguments still collect';

# A genuinely lazy source must NOT be reified into the slurpy — it stays lazy
# and reify-on-index, or an infinite source would hang here.
sub head3(*@a) { @a[0 .. 2].join(',') }
is head3(1 .. Inf), '1,2,3', 'control: an infinite lazy source stays lazy in *@a';

# A non-slurpy positional receives the Seq itself, not its elements.
sub whole(@a) { @a.elems }
is whole((1..3).map(* + 0)), 3, 'control: a non-slurpy @a receives the Seq intact';

# --- still open: a `gather` / sequence-operator Seq binds as ONE element ---
#
# A different mechanism from the map/grep hole above: the slurpy is not empty,
# it holds the Seq itself (`@a[0].^name` is `Seq`, `@a[0].elems` is 4), so
# `flatten_into_slurpy` is declining to flatten it rather than reading an empty
# seed. Tracked in todo/tickets/lazy-seq-argument-vanishes-into-a-user-slurpy.md.
todo 'a gather Seq is bound as one element instead of being flattened', 1;
is count(gather { take $_ for 1 .. 4 }), 4, 'gather into *@a flattens';
