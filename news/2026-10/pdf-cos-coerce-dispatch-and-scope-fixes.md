# PDF's coercion suite: six dispatch, coercion and scoping fixes

Running PDF 0.6.17's `t/cos-coerce.t` turned up six independent bugs, all
fixed here, taking it from dying in subtest 4 to the `dict sub-types` subtest:

- A multi candidate's `where` clause that smartmatches a regex
  (`where PDF::COS::TextString.COERCE($_) ~~ DateRegex`) now leaves its `$/`
  for the candidate's body, as an only sub always did. Resolution ran the
  clause in a scope that discarded `$/`, so `$<year>` read `Nil`.
- A role type object argument (`R`, with `role R does P`) ranks a `P`
  parameter narrower than an untyped one. Both used to score as unrelated to
  the role, so `multi m($o, P $c)` tied `multi m(Any:D $o, $t)`.
- `my T() $x = Any` hands the type object to `T.COERCE`, as parameter
  binding already did, instead of failing the assignment type check.
- A `unit class C is P;` / `unit role R does Q;` body keeps its `use`
  statements: the hoisted copy only loads the module early. The hoisted
  import alone landed in `GLOBAL`, and a scope rollback around the module's
  first load (a `require` in a parameter default) dropped the class's
  imported multi.
- `self.^is_pun` / `self.^pun_source` in a method called on a role type
  object answer for the role's pun.
- A `sub` installed with `^add_method` keeps the `self` of the method that
  created it; only its first parameter is the receiver.
