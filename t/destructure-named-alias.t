use Test;

plan 8;

# Named alias (`:outer(:$inner)`) inside a sub-signature destructure
# (JSON::Unmarshal's `_unmarshall-context` signature shape).
my sub f(% (Bool :$opt-in, Bool :die(:$throw), *%extra)) {
    ($throw // Bool).gist ~ "|" ~ %extra.keys.sort.join(",");
}

is f({die => True, foo => True}), "True|foo",
    'outer alias key binds inner name; consumed key excluded from slurpy';
is f({throw => True}), "True|",
    'inner alias key also accepted';
is f({opt-in => True, bar => 1}), "(Bool)|bar",
    'unset alias stays undefined; plain named consumed from slurpy';

# Multiple named params all consumed from the named slurpy.
my sub g(% (Bool :$a, Bool :$b, *%rest)) {
    %rest.keys.sort.join(",");
}
is g({a => True, b => True, c => 1, d => 2}), "c,d", 'plain named params consumed';

# The plain-rename form still binds and is excluded from the slurpy.
my sub h(% (Int :count($n), *%rest)) {
    "$n|" ~ %rest.keys.sort.join(",");
}
is h({count => 3, x => 1}), "3|x", 'rename form binds inner name';

# Unset alias inner names are still declared (no X::Undeclared).
my sub k(% (Bool :die(:$throw))) { ($throw // Bool).gist }
is k({}), "(Bool)", 'unset alias inner name is declared';
is k({die => True}), "True", 'set via outer';
is k({throw => True}), "True", 'set via inner';
