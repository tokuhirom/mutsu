# A routine exported by a run-time `sub EXPORT` now parses as a listop

mutsu's parser pre-scans each `use`d module for the names it exports, and that
scan is what lets an imported name parse as a listop — knowing `root` is a
routine is the difference between `root <abcd abce>` being a call with a
quote-word argument and being an infix `<` comparison against a bareword.

The scan only ever looked for `is export` traits. A module that computes its
exports at load time through Raku's `sub EXPORT` hook carries no such trait, so
the scan came back empty and the importer's parse learned nothing about it. Every
call shape that needs the parser to know the name is a routine was then a hard
parse error:

```raku
use String::Utils;
say root <abcd abce>;   # raku: ab     mutsu: ===SORRY!=== Confused.
```

The parenthesised form `root(<abcd abce>)` worked, and the same listop shape
worked for a locally-declared `sub root`, which located the gap precisely.

Fixing it properly would mean knowing a module's exports at parse time, and those
names exist only once the module has run — rakudo gets them because it loads
compunits at BEGIN time, and mutsu loads modules at run time. Rather than start
that campaign for one parse gap, the scan now *approximates*: when a module
declares a unit-scope `sub EXPORT`, its unit-scope routine declarations are
registered as the importer's imported callables. That is verbatim what the
dominant idiom exports —

```raku
my sub EXPORT(*@names) {
    Map.new: UNIT::.grep: { .key.starts-with('&') && !(.key eq '&EXPORT') }
}
```

— and every other hook shape draws its `Pair` values from the same pool of
unit-scope routines.

The approximation is a superset, and that asymmetry is deliberate. The set is
parse-time knowledge only: it lands in `Scope::imported_functions`, whose every
consumer is a parser decision about whether an identifier is a routine. Run-time
name resolution still resolves against the import set produced by actually
running `sub EXPORT`. So naming a routine the hook withholds costs a worse
diagnostic at worst and can never change the meaning of a program that runs,
while registering nothing — the old behavior — made the call shape unparseable.
The gate on "this module declares `sub EXPORT`" confines the approximation to
exactly the modules whose current answer is the empty set.

`String::Utils` is the dist this was measured on: its `t/01-basic.rakutest` uses
the listop shape on eight lines and previously died during parsing with zero
assertions run, against a 124-assertion `raku` baseline. It now reaches 117
passing assertions, with the residue being unrelated run-time bugs rather than
parsing.

The decision, the three alternatives weighed against it (parse-time compunit
loading, evaluating the hook alone, and a use-site `<...>` guess), and the
soundness argument for the superset are recorded in
[ADR-0087](../../docs/adr/0087-runtime-export-hook-parse-time-approximation.md).
Pinned by `t/modules/import-export/runtime-export-listop-parse.t`, which checks
both directions: the listop call parses, and a unit-scope routine the hook
deliberately withholds stays unresolvable at run time.
