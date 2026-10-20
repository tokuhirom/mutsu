# ADR-14032: Built-in introspection reads Rakudo's method-table snapshot, gated by the method rows

- **Status**: Accepted (user decision 2026-10-20)
- **Date**: 2026-10-20
- **Deciders**: tokuhirom, Claude
- **Issue**: [#14032](https://github.com/tokuhirom/mutsu/issues/14032)
- **Related**: [ADR-11276](11276-built-in-methods-are-handler-rows.md) (§2.3: `.^can` and
  `.^methods` read the method table; §6 item 5: delete `native_method_row_table.rs`), #11271
  (the drift the recognition table caused), #10234 (the `augment` conflict), #11405 (the oracle
  checks taken off the gate)

## 1. Context

ADR-11276 slice 5 deletes the recognition table, `src/builtins/native_method_row_table.rs`:
2,808 `(owner, name, arity, flags)` rows baked from Rakudo once (2026-08-10, #9869) and
hand-maintained since. Its arity bits and three of its flag bits are already dead. What it still
answers are four facts about Rakudo:

| flag | meaning | readers |
|---|---|---|
| `DECLARED` | the name is a key of `::(owner).^method_table` | `.^can`, `.^find_method`/`.^lookup` per MRO level, `.^methods` of the classes whose triage is incomplete, the enum dispatch check |
| `INTROSPECTABLE` | `::(owner).^methods` lists the name | `.^methods`, the methods of composed roles |
| `ONLY_METHOD` | the declaration is an `only` method | the compile-time `augment` conflict (#10234) |
| `REVISION_E` | language revision 6.e adds the name | every reader, under an older revision |

`src/builtins/rakudo_method_tables.txt` records all four for 110 owners (`declared`, `methods`,
`only` and their `.e` forms). It is generated from a real Rakudo by
`scripts/gen-rakudo-method-tables.raku` and is today only the oracle a test checks the table
against. The table is a strict subset of it: it holds the pairs mutsu was found to answer. So the
snapshot cannot simply replace the table: `.^can` would then claim Rakudo-internal methods mutsu
has no implementation for (`Block.fire_phasers`, `Any.iterator-and-first`).

ADR-11276 §2.3 says `.^can` and `.^methods` read the method table. A row says that mutsu answers a
name and on which owner Rakudo declares it, but not whether Rakudo's `.^methods` lists it, whether
it is an `only`, or what Rakudo declares that mutsu does not answer. Those facts need a source.

Measured on 2026-10-20 (`main` plus #14031) with the rule of §2 (the snapshot's answer for the
owners it lists, the rows' alone for the others, and in both cases only where a row exists):

- `DECLARED` would turn false on 54 pairs: `Blob` 21 (the folded `Buf`/`Blob`/`utf8` owner of
  ADR-11276 §8.3, whose rows sit at `Buf`), `Enumeration` 7, `Failure` 4, RakuAST nodes 4,
  `Iterator` 3, `Mu` 3 (`WHAT`/`HOW`/`WHY`, compiler pseudo-methods), the quant hashes'
  `BIND-KEY` 6, and single pairs (`Cool.Int`, `Junction.Bool`, `IO::Path.path`, ...). Each is a
  method mutsu answers through a row at a different owner, a cascade arm or the compiler.
- It would turn true on 84 pairs: `Buf` 57 and `FatRat` 24 (rows at the owner Rakudo uses, which
  the recognition table folded into `Blob` and `Rat`), and three single pairs.
- The snapshot has no line for the roles (`Real`, `Numeric`, `Iterator`, `Enumeration`, `Blob`,
  ...): the generator walks classes. Rows at those owners therefore answer alone until the
  generator learns roles.

## 2. Decision

1. **The snapshot is the source of Rakudo facts at run time.** `rakudo_method_tables.txt` is
   compiled into the binary (`include_str!`) and parsed on the first introspection call, never on
   the startup path (ADR-11276 §2.6). It stays generated, never hand-edited; extending it to an
   owner it lacks means re-running the generator.
2. **The method rows are the source of existence.** `.^can(name)`, `.^find_method`,
   `.^lookup` and `.^methods` on a built-in owner answer a name only when the snapshot has it for
   that owner (`declared`, or `methods` for `.^methods`) **and** a method row reaches the name
   from that owner along its MRO. An owner the snapshot does not list (a class mutsu models that
   the generator never saw) answers from its rows alone, with the row's owner as the declaring
   type.
3. **The `augment` conflict reads the snapshot alone.** `augment class Str { method uc {} }` is
   a compile-time error in Rakudo whether or not mutsu implements `Str.uc`, so the `only`/
   `declared` facts are not gated by the rows.
4. **Language revision** comes from the snapshot's `.e` lines, as `REVISION_E` rows of the method
   table already do.
5. **The recognition table is deleted once the measured gap is zero,** and the gap is closed by
   registering rows at the owners Rakudo declares the methods on (the `Buf`/`Blob` split of
   ADR-11276 §8.3, `Enumeration`, `Iterator`), by extending the generator to the roles, or by an
   explicit row for a compiler pseudo-method, never by keeping a second table. The measurement
   becomes a test that lists the pairs whose answer changes; it shrinks to empty, and the switch
   is the commit that deletes the table.

## 3. Consequences

- One generated file and the method rows answer every introspection question; nothing is
  hand-maintained. A new row makes `.^can` true by itself, so the #11271 drift cannot recur.
- Introspection moves toward Rakudo: `FatRat.^can` and `Buf.^can` answer at the owners Rakudo
  uses, and `.^methods` lists names Rakudo lists that the 2026-08 hand triage missed.
- Before the switch, the 54 pairs are real work (mostly `Blob`/`Buf` and `Enumeration`). That
  work is ADR-11276's anyway: a method answered by a cascade arm is a slice-5 leftover.
- `.^lookup(...).package` can name the declaring role (`Str.^lookup('uc').package` is `Cool` in
  Rakudo), because the row's owner is Rakudo's declaring type; today it names the receiver's
  class.
- The snapshot is a build input: regenerating it against a newer Rakudo can change mutsu's
  introspection answers. That is intended (it tracks Rakudo), and the regeneration is reviewed as
  a diff of the text file.

## 4. Alternatives rejected

- **Keep the table, regenerated by a script from the snapshot and the rows.** One source in
  principle, but a 3,000-line generated file that every row-adding PR must regenerate, and a
  conflict point between parallel PRs (the reason ADR-11276 §10.3 moved shared counters out).
- **Rows only, no snapshot.** Loses `only` (the `augment` conflict), Rakudo's `.^methods`
  listing (mutsu registers rows Rakudo does not list, such as argument-form twins), and the
  declarations mutsu does not implement.
- **The snapshot only.** Claims methods mutsu cannot run (`.^can` true, then
  `X::Method::NotFound`), which is worse than the drift #11271 fixed.

## 5. Open questions

- Whether a compiler pseudo-method (`WHAT`, `HOW`, `WHY`, `VAR`) gets a row whose handler is
  never called, or an explicit list next to the snapshot reader.
- How the folded owners of ADR-11276 §8.3 (`Buf`/`Blob`/`utf8`, `Sub`/`Method`/`Block` to `Code`)
  map onto the snapshot's real owners; this ADR assumes that question is settled by giving them
  rows at Rakudo's owners.
