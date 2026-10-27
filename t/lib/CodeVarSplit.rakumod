# Mirrors Timezones::ZoneInfo::State: a `my &code` the class body assigns
# after a method that calls it and declares a class of its own.
unit class CodeVarSplit is export;
my &fmt;
method go(Str $s) { class Inner { }; fmt($s) }
&fmt = sub (Str $n) { $n ~ "!" };
