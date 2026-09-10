use Test;

plan 4;

# `does Qualified::Role::Name` as a trait on a `unit role` / `unit class`
# declaration must parse as a single DoesDecl. Previously the qualified name
# tripped the token-name parser at the `::`, so `does DBDish::ErrorHandling`
# was split into two bare-word statements and the role was never composed --
# its attributes (e.g. `$.parent`) never reached the consuming class. This
# blocked DBIish (DBDish::ErrorHandling -> DBDish::StatementHandle -> driver
# StatementHandle classes).

role My::Base { has $.parent is required; }
role My::Mid does My::Base { has $.mid; }
class My::Leaf does My::Mid {
    has $.leaf;
    submethod BUILD(:$!parent!, :$!mid, :$!leaf) { }
}

my @names = My::Leaf.^attributes.map(*.name);
ok '$!parent' (elem) @names, 'transitively-composed $!parent reaches the class';
ok '$!mid'    (elem) @names, 'directly-composed $!mid reaches the class';

my $obj = My::Leaf.new(parent => 'P', mid => 'M', leaf => 'L');
is $obj.parent, 'P', 'attribute from a qualified-name role is usable';
is $obj.mid, 'M', 'attribute from the qualified mid role is usable';
