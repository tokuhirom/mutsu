use Test;

plan 6;

# Rakudo's metamodel classes and metaroles are named `Perl6::Metamodel::X`;
# `Metamodel::X` is only the alias they are reachable under.
is Metamodel::ClassHOW.^name, 'Perl6::Metamodel::ClassHOW', '^name of a HOW class';
is Metamodel::ClassHOW.raku, 'Perl6::Metamodel::ClassHOW', '.raku of a HOW class';
is Metamodel::ClassHOW.gist, '(ClassHOW)', '.gist keeps the short name';
is Metamodel::Naming.^name, 'Perl6::Metamodel::Naming', '^name of a metarole';
is Metamodel::Primitives.^name, 'Metamodel::Primitives', 'Primitives is not renamed';
is class { }.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'a class HOW instance already was';
