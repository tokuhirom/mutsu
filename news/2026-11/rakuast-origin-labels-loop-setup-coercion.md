# RakuAST: labels, C-style loop setups, definite coercions, shadowed setting types

More `.origin` and shape gaps closed (#15780):

- a statement visits its labels last, as Rakudo's `visit-children` does
  (`visit_order::reorder`), and each `Label` is placed over its `NAME:`; a
  labelled expression statement (`R: say 1`) is placed after its label, and
  a label named as an argument (`last L`) is a placed `Term::Name`
  (`label_origins`);
- a C-style loop's setup (`loop (my $i = 0; ...)`) is placed from the text
  between the `(` and the first `;` (`loop_setup_origins`);
- `Int:D(Str)` places its inner `Type::Simple` over the whole coercion and
  its `Name` over `Int`, leaving the `Type::Definedness` unplaced, as Rakudo
  does (`type_origins::coercion`);
- a sigilless term declared in a block (`my \uint8 = 3`, `my \Int = 4`)
  shadows a setting type there: a later bareword is its `Term::Name`, not a
  `Type::Simple` (`bareword::BlockScope`).
