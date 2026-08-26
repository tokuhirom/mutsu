# The 6.e `rotor` subroutine

`Type/List.rakudoc` documents `rotor` twice: as the `List.rotor` method, which
mutsu already implemented, and — "from language version 6.e onward" — as a
subroutine whose signature puts the list last:

```
multi rotor(Int:D  $batch, \thing, Bool() :$partial --> Seq:D)
multi rotor(Pair:D $batch, \thing, Bool() :$partial --> Seq:D)
multi rotor(**@cycle,      \thing, Bool() :$partial --> Seq:D)
```

mutsu had no `rotor` sub at all, so `say rotor(3, 'a'..'h')` died with
`Unknown function: rotor` even under `use v6.e.PREVIEW`.

## Is it a core builtin?

`CLAUDE.md` says to implement a function as a builtin only if it appears in
`raku-doc/doc/Language/perl-func.rakudoc`. **`rotor` does not appear there** —
but that file is the *Perl 5 `perlfunc` migration table* ("Alphabetical listing
of Perl functions"), not an index of Raku builtins, and it lists no Raku-only
routine of this kind. The rule's actual purpose — do not mistake a module
function (`Test`, `Test::Util`) for core — is satisfied here by direct evidence:
`raku -e 'use v6.e.PREVIEW; say rotor(3, 1..6)'` resolves `rotor` with no module
loaded, and dropping the pragma turns it into `Undeclared routine: rotor`. It is
a language-version-gated CORE routine, so it belongs in core.

## Implementation

`builtin_rotor` (`src/runtime/builtins_collection_deepmap.rs`) splits off the
`:partial` adverb, pops the trailing `\thing` positional, and delegates to
`thing.rotor(@cycle, :$partial)` — one implementation of the cycle semantics
(`Pair` gaps and overlaps, cycling a multi-element spec, `:partial`), not two.

The 6.e gate is applied at the call, against
`crate::parser::current_language_version()`: in a 6.c/6.d compilation unit
`rotor(...)` reports `Undeclared routine: rotor -- the rotor subroutine needs
`use v6.e.PREVIEW``. (Rakudo rejects it at compile time; mutsu's diagnostic
arrives at the call, which is the same message for the same program.)

Pinned by `t/range-bounds-and-rotor.t`, which passes verbatim under both `raku`
and mutsu.
