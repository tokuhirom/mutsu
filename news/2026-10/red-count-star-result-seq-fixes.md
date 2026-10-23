# Red's `count(*)` result sequences: five interpreter gaps closed

`Model.^all.elems` / `.grep(...).elems` died in Red 0.2.7 with "Impossible coercion from
'Any' into 'Int'". Tracing it through the real module found five independent gaps, each
now fixed and pinned by its own test:

- **`:&.name` colonpair** passed `Nil` instead of the accessor's callable (`:&.post` is
  `post => &.post`, the term form that already worked), so `Red::ResultSeq::Iterator`
  never received its `&.post` row transform.
- **`.Seq` on a `but`-mixin value** was answered natively as a one-element Seq, skipping a
  user-defined `Seq` method. Red's `create-map` returns `self but role CMModel[...]`, so
  `do-it(...).Seq` never reached `ResultSeq.Seq`.
- **A branch's own `my $*x`** shadows an enclosing dynamic variable for the branch only.
  In `if`/`with`/`given` bodies, including their value-position `do if`/`do with` forms, the
  declaration leaked past the branch, so `$*RED-INSIDE-COMPUTATION` stayed set and the
  aggregate column lost its `as "data_1"` alias. Such branches now run as real blocks.
- **`^add_attribute` with a role-mixed Attribute** (`$attr does Red::Attr::Column(...)`)
  dropped the role, so `.^attributes.grep(Red::Attr::Column)` came back empty inside
  `compose`. The mixed object is kept.
- **An explicit invocant type** (`multi method m(RM:D:)`) was not part of the narrowness
  order, so `(RM:D:)` tied with `(Mu:D:)` and the call was ambiguous (rakudo picks the role).

Refs #14094.
