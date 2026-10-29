use Test;

plan 6;

# A Rakudo metarole is an NQP-level object whose HOW has no find_method:
# rendering it (or a junction of them) dies, which Kind's naming relies on to
# fall back to the type name.
throws-like { Metamodel::Naming.raku }, Exception, message => /find_method/,
    '.raku of a metarole dies';
throws-like { Metamodel::Naming.gist }, Exception, message => /find_method/,
    '.gist of a metarole dies';
throws-like { (Metamodel::Naming & Metamodel::Versioning).raku }, Exception,
    message => /find_method/, '.raku of a junction of metaroles dies';
is Metamodel::Naming.^name, 'Perl6::Metamodel::Naming', '.^name still names it';
is (try Metamodel::ClassHOW.raku), 'Perl6::Metamodel::ClassHOW', 'a HOW class still renders';
is (Metamodel::Naming & Metamodel::Versioning).^name, 'Junction', 'the junction names its own type';
