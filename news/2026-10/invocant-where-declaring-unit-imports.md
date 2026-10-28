# A multi method's invocant `where` resolves the declaring unit's imports

The invocant `where` check added for `multi method m(\SELF where { ... }:)` ran
outside the candidate's declaring compunit, so a type only that unit imported
(Red's `Json` in `ColumnMethods.contains`) died with `Undeclared name`. It now
runs under `with_candidate_unit` like the rest of the signature match, which
fixes `*.name.contains: "ll"` on a Red column (t/39-xmas).
