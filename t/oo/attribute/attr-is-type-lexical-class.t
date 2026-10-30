use Test;

# `has @.a is Type` builds an instance of Type even when Type is a lexical
# (`my class`) declared in the enclosing scope: the container type is resolved
# where the lexical is visible, not by bare name at construction time.

plan 5;

my class Bag2 does Positional {
    method POINT(*@a) { "point:" ~ @a.join(",") }
}
my class Holder {
    has @!idents is Bag2;
    has @.pub is Bag2;
    has %.h is Bag2;
    method go { @!idents.POINT(1, 2) }
    method kind { @!idents.^name }
}

my $h = Holder.new;
is $h.kind, 'Bag2', 'a private @-attribute is an instance of the lexical class';
is $h.go, 'point:1,2', 'its methods dispatch';
is $h.pub.^name, 'Bag2', 'a public @-attribute too';
is $h.h.^name, 'Bag2', 'and a %-attribute';

class Plain { has @.p is Bag2 }
is Plain.new.p.^name, 'Bag2', 'a package class using the lexical type';
