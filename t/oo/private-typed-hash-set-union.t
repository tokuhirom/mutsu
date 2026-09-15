use Test;

# A private object-hash attribute must use the value side of its declaration
# (`Any` here) when checking a whole-container assignment.  The `{Attribute}`
# part constrains keys; it must not reject the Bool values produced by `∪=`.

plan 4;

role R {
    has %!relationships{Attribute};

    method add(Attribute $attr) {
        %!relationships ∪= $attr;
        %!relationships
    }
}

class C does R { }

my $attr = Attribute.new(:name<foo>, :type(Any), :package(::?PACKAGE));
my $relationships;
lives-ok { $relationships = C.new.add($attr) },
    'union assignment to a private typed object hash lives';
is $relationships.elems, 1, 'the union stores one relationship';
my $key = $relationships.keys.head;
isa-ok $key, Attribute, 'the object-hash key keeps its declared type';
isa-ok $relationships{$key}, Bool, 'the set-union value is accepted by Any';
