use Test;

# `$attr does Role` on an Attribute built for a `new_type` type is recorded
# against the owning type. Two types minted under one name are different types
# (Red's `^specialise` builds a fresh `new_type(:name($self.^name))` per
# projection), so their attributes must not share a registry slot: the second
# type's `^attributes` used to hand back the first type's attribute, and every
# later `.map(*.other-column)` of a Red model saw the first projection's column
# type (Red 0.2.7 t/03-blog, t/89-sql-tail, t/91-cross-product).

plan 7;

role AttrCol { has %.args; method tag { %!args<tag> } }

sub mint(\type, $tag) {
    my \m = Metamodel::ClassHOW.new_type(:name<Q>);
    my $attr = Attribute.new(:name('$!d'), :package(m), :type(type), :has_accessor);
    $attr does AttrCol(%(:$tag));
    m.^add_attribute($attr);
    m.^compose;
    m
}

my \first  = mint(Str, 'first');
my \second = mint(Set, 'second');

my $a1 = first.^attributes.first(*.name eq '$!d');
my $a2 = second.^attributes.first(*.name eq '$!d');

is $a1.type.^name, 'Str', 'the first type keeps its attribute type';
is $a2.type.^name, 'Set', 'the second type has its own attribute type';
is $a1.tag, 'first', 'the first attribute keeps its role arguments';
is $a2.tag, 'second', 'the second attribute has its own role arguments';
ok $a1.package === first, 'the first attribute reports its own type as package';
ok $a2.package === second, 'the second reports its own';
nok $a1 === $a2, 'the two attributes are different objects';
