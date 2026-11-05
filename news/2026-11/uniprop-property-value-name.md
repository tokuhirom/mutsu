# `uniprop` reads a bare property value name as the property it belongs to

`'中'.uniprop('W')` answered the General_Category (`Lo`) because `W` is not a
property name, so a display-width helper that asks `uniprops('W')` counted every
East Asian wide character as one column. rakudo reads the name as a property
value: a Script value (`Latin`, `Han`, ...) answers the Script, an
East_Asian_Width value (`W`, `Na`, `F`, `H`) answers the East_Asian_Width, and
a General_Category value or anything unknown still answers the category.
App::DuckETL's `t/06-render.rakutest` passes (44/44, as on rakudo). Bidi_Class
value names are not covered yet.
