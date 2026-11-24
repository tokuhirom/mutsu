use lib 't/lib';
use Test;
use DeclareShout;

# Ranges measured on Rakudo 2026.11. An unscoped custom package starts at
# its name; an explicit scope keeps the scope keyword in the package origin.
plan 3;

sub class-range(Str $source) {
    my $class = $source.AST.statements[1].expression;
    "{$class.origin.from}-{$class.origin.to}"
}

is class-range(Q[use DeclareShout; shouter Loud { }]), '26-34',
    'an unscoped custom declarator starts the class origin at its name';
is class-range(Q[use DeclareShout; my shouter Quiet { }]), '18-38',
    'a lexical custom declarator starts the class origin at its scope';
is class-range(Q[use DeclareShout; our shouter Public { }]), '18-40',
    'a package-scoped custom declarator starts the class origin at its scope';
