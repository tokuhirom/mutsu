unit module RebindOuterLexicalUnitModule;

# The `unit module` flavour of #11797: the variable is a unit lexical.
my $buf := [1, 2];

our sub unit-add() { $buf.push(3) }

our sub unit-take() {
    my $new := $buf;
    $buf := [];
    $new.elems
}

our sub unit-current() { $buf.elems }
