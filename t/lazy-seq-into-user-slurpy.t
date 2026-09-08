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
#
# A `gather` / sequence-operator argument had a SECOND mechanism: it is a
# LazyList rather than a `Seq` body, so it matched none of the binder's iterable
# arms and arrived as ONE slurpy element holding the whole sequence. Forcing it
# needs the VM -- a gather body is user code -- which is why the pure
# `flatten_into_slurpy` could not do it. Only a *genuinely* lazy source stays
# whole, or an infinite one would hang.

plan 31;

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

# --- the gather / sequence-operator producers (the LazyList half) ---

is count(gather { take $_ for 1 .. 4 }), 4, 'gather into *@a flattens';
is items(gather { take $_ for 1 .. 4 }).raku, '[1, 2, 3, 4]',
    'gather into *@a flattens to its elements, not to one Seq';
is count(1, *+1 ... 4), 4, 'the sequence operator into *@a flattens';
is count(gather {}), 0, 'an empty gather contributes no elements';

# The same four producers alongside other arguments: a slurpy collects every
# top-level argument, so the flattened sequence has to compose with them.
is count((gather { take $_ for 1 .. 4 }), 9), 5, 'gather flattens next to a plain argument';
is count((1, 2), gather { take $_ for 1 .. 4 }), 6, 'gather flattens after a list argument';
is count((1, *+1 ... 4), 9), 5, 'a sequence flattens next to a plain argument';
is count((1..3).map(* + 0), gather { take 9 }), 4, 'a .map Seq and a gather flatten together';

# --- the same four producers into a non-slurpy `@a` ---
#
# The two paths used to disagree: `@a` reified all four while `*@a` dropped or
# boxed two of them.

is whole((1..3).grep(* > 0)), 3, 'a .grep Seq into a non-slurpy @a';
is whole(gather { take $_ for 1 .. 4 }), 4, 'a gather into a non-slurpy @a';
is whole(1, *+1 ... 4), 4, 'a sequence into a non-slurpy @a';

# --- and into a `+@a` "one-argument rule" slurpy ---

sub plus-count(+@a) { @a.elems }

is plus-count((1..3).map(* + 0)), 3, 'a .map Seq into +@a flattens';
is plus-count((1..3).grep(* > 0)), 3, 'a .grep Seq into +@a flattens';
is plus-count(gather { take $_ for 1 .. 4 }), 4, 'a gather into +@a flattens';
is plus-count(1, *+1 ... 4), 4, 'a sequence into +@a flattens';
is plus-count((1, 2), gather { take $_ for 1 .. 4 }), 2,
    'control: +@a does NOT flatten when there is more than one argument';

sub plus-name(+@a) { @a.^name }
is plus-name(1 ... *), 'List', 'control: an infinite source into +@a stays lazy';

# --- laziness controls: a genuinely lazy source must never be forced ---
#
# Forcing any of these would hang, so they are the guard rail on the fix: only a
# LazyList that reports itself finite is reified into the slurpy.

sub lazily(*@a) { @a.is-lazy }

is head3((1..Inf).map(* + 1)), '2,3,4', 'control: an infinite .map pipe stays lazy in *@a';
is lazily(1 .. Inf), True, 'control: an infinite Range slurpy still reports .is-lazy';
is lazily(1 ... *), True, 'control: an infinite sequence slurpy still reports .is-lazy';
