# App::Stouch: block-scoped MAIN modules and quoted braces in regexes

Three gaps found by taking `App::Stouch`'s own suite from red to green:

- A `unit module` that declares `multi sub MAIN` lost its package-qualified
  candidates after loading, so `App::Stouch::MAIN(...)` died with "Routine does
  not have any candidates". Only package-less `MAIN`s are program-MAIN leaks.
- A script's own `BEGIN sub MAIN(|) { }` followed by `{ use Module-exporting-MAIN; }`
  raised "Redeclaration of routine", and the block-imported candidates stayed
  registered after the block ended, so the program printed a usage message and
  exited 2. A module's `MAIN` is now lexical to the block that imported it.
- A single-quoted literal containing a brace (`/ '{{' $k '}}' /`) was mistaken for a
  code block when interpolating scalars, so `$k` was never substituted.
