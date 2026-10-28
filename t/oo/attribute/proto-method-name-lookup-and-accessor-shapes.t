use Test;

# lookup_proto_method answers "no proto of this name anywhere" from a set of
# proto method names, and the @/% accessor read keeps only an attribute's
# shape: neither may change what a call or a read observes.

plan 14;

class Base {
    proto method pick(|) {*}
    multi method pick(Int $x) { "int:$x" }
    multi method pick(Str $x) { "str:$x" }
    method plain($x) { "plain:$x" }
}
class Derived is Base {
    multi method pick(Rat $x) { "rat:$x" }
}
class Unrelated {
    method pick($x) { "unrelated:$x" }
}

is Base.new.pick(1), "int:1", 'proto dispatches to the Int candidate';
is Base.new.pick("a"), "str:a", 'proto dispatches to the Str candidate';
is Derived.new.pick(1), "int:1", 'an inherited proto still reaches the base candidates';
is Derived.new.pick(0.5), "rat:0.5", 'a derived multi joins the inherited proto';
is Unrelated.new.pick(7), "unrelated:7", 'a same-named method of a class without the proto is untouched';
is Base.new.plain(3), "plain:3", 'a method no proto declares runs normally';

class Holder {
    has @.list is rw = 1, 2, 3;
    has %.map is rw = a => 1;
    has @.ro = 4, 5;
    has $.scalar = 9;
    has Int @.typed = 1, 2;
}
my $h = Holder.new;

is $h.list.elems, 3, 'an is-rw array accessor reads its elements';
$h.list.push(4);
is $h.list.elems, 4, 'an is-rw array accessor hands out the live container';
$h.map<b> = 2;
is $h.map.keys.sort.join(","), "a,b", 'an is-rw hash accessor hands out the live container';
is $h.ro.join(","), "4,5", 'a read-only array accessor reads its elements';
is $h.scalar, 9, 'a scalar accessor is unchanged';
dies-ok { $h.typed.push("x") }, 'a typed array accessor keeps its element type';

# A declared class is a type in its own right, not an alias to chase.
class A::B::Thing { }
my A::B::Thing $t = A::B::Thing.new;
isa-ok $t, A::B::Thing, 'a typed declaration of a nested class';
dies-ok { my A::B::Thing $u = 5 }, 'the declared class constrains the variable';
