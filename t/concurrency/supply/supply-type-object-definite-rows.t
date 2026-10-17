use Test;

# `Supply`'s `Supply:D:` methods refuse the type object through their rows
# (ADR-11276, `RowFlags::DEFINITE`), not through a list of names; the
# `multi` ones find no candidate (`RowFlags::DEFINITE_MULTI`).

plan 8;

for <tap skip max wait> -> $m {
    throws-like { Supply."$m"() }, X::Parameter::InvalidConcreteness,
        "Supply.$m refuses the type object";
}
throws-like { Supply.map({ $_ }) }, X::Parameter::InvalidConcreteness,
    'Supply.map with a block';
throws-like { Supply.lines }, X::Multi::NoMatch, 'Supply.lines';
throws-like { Supply.rotor(2) }, X::Multi::NoMatch, 'Supply.rotor';
ok Supply.from-list(1, 2).list eqv (1, 2), 'a class method still answers';
