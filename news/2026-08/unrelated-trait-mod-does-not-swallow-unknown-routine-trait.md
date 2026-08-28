# An unrelated `trait_mod:<is>` candidate no longer swallows an unknown routine trait

In Raku, built-in `trait_mod:<is>` candidates live in the same multi as any
user-declared one, so a user candidate whose signature (including a
required named parameter's specific NAME) does not match a given `is TRAIT`
simply does not claim it — dispatch falls through, and an unrecognised
trait still raises `X::Comp::Trait::Unknown` ("Can't use unknown trait").

A 2026-08-01 fix (`f58c424c6`, "a user trait_mod:<is> that matches nothing
keeps the trait") made this work correctly for *variable* traits (`my $a is
whatever`), but explicitly left *routine* traits (`sub f() is whatever {
}`) unfixed — its own commit message called this out: "The same shape for
routine traits is a different code path and mutsu does not dispatch there
at all today ... neither runs the handler nor errors."

That gap is now closed. `src/runtime/registration_sub.rs`'s routine-trait
application swallowed the `trait_mod:<is>`-dispatch "no candidate matched"
verdict unconditionally (`Err(e) if Self::is_trait_mod_no_candidate(&e) =>
{}`), leaving the sub declared with the unknown trait silently dropped —
never running a handler, never raising an error. `has_trait_mod` (checked
just above, guarding the sibling `!has_trait_mod` branch) only proves *some*
`trait_mod:<is>` multi exists anywhere — e.g. merely `use Test;` supplies
`multi sub trait_mod:<is>(Routine:D, :$test-assertion!)` — it says nothing
about whether any candidate actually claims a *given* trait. The fix mirrors
the sibling `!has_trait_mod` branch exactly: a genuine no-candidate verdict
now raises the same "Can't use unknown trait 'is' -> '...' in sub
declaration." message (still exempting `test-assertion`, mutsu's own
built-in meaning, and still gated on being inside `EVAL` — outside EVAL, a
handler may simply not be registered yet during ordinary module loading,
matching the pre-existing tolerance of the sibling branch).

Found while closing out `roast/S14-traits/routines.t`'s `MUTSU_REAL_TEST=1`
residue: merely `use Test;` (which exports exactly the `:$test-assertion!`
candidate named above) made `EVAL 'sub yulia is krassivaya { }'` silently
succeed instead of dying — so `try { EVAL '...' }` never set `$!`, and the
subtest asserting `"$!" ~~ /'unknown trait'/` failed with "Use of
uninitialized value ... in string context" instead. Pinned by
`t/unrelated-trait-mod-candidate-does-not-swallow-unknown-trait.t`, green
under `raku` too. Closes `roast/S14-traits/routines.t` under
`MUTSU_REAL_TEST=1` (its only other failure, "and the wrapper has been
called once", is `#?rakudo todo`-marked and does not count).
