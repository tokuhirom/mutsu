# A bare regex never starts a match against an undefined topic

`/x*/` in statement or boolean position with `$_` unset matched the empty string in mutsu (`say so
/x*/` was `True`, and `/{ say "ran" } x/;` ran the code block). Rakudo does not start the match at
all: the result is `False`/`Nil`, `$/` is untouched and no code block runs. `m/.../` and
`$_ ~~ /.../` are explicit matches and keep matching (with the uninitialized warning), so
`compile_match_regex_topic` now marks only the bare form as an implicit topic. A block that is the
program's last statement and ends in a bare regex statement still does not match a defined topic;
that is a separate gap. Found by the raku-doc differential (`Language/regexes.rakudoc:2580`).
