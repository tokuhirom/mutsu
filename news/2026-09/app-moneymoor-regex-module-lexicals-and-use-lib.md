# App::Moneymoor: module lexicals in regexes, and `use lib $?FILE...`

Three interpreter gaps surfaced by `App::Moneymoor` 0.4.2's suite are fixed.

A regex inside a module's `our sub` could not interpolate the module's own
file-scope `my` (`my Str $current-decimal = '.'; ... / $current-decimal /`).
The regex literal's closure capture read only `env`, while plain reads of the
same name resolve through the escaped-`our`, package-block and compunit
lexical stores; the capture now uses the same chain. Kebab-case names are
captured under their full spelling from those stores only, so the existing
live match-time reads of same-frame kebab lexicals are unchanged. Relatedly,
the scratch interpreter that runs regex code (`$(...)`, `<{...}>`) now shares
those stores, so a module routine called from regex code sees its own
lexicals.

`use lib $?FILE.IO.parent.add('lib').Str` is now folded at parse time (and in
the pre-execution type check, through the same function), so a module found
through it is imported while the rest of the file is parsed: its exported
classes are known to later signatures (`sub f(--> Facts)`), and its exported
constants parse as terms inside `?? !!`.

App::Moneymoor moved from 20/30 to 29/30 baseline files at parity; the last
one, `t/91-boot-pipeline.rakutest`, needs in-place `SetHash` mutation (#9609).
