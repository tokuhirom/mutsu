use Test;

# Assigning a hash or array to a typed container attribute checks every element
# against the declared element type. The check is remembered per class within
# one assignment; a later element of another class must still be rejected, and
# a subclass or a role-doing class must still be accepted.

plan 13;

role Tr { }
class A does Tr { }
class B does Tr { }
class C { }
class SubA is A { }
class Base { }
class Kid is Base { }

class H {
    has Tr %.h;
    has Tr @.a;
    has Base %.b;
    method seth(%x) { %!h = %x; self }
    method seta(@x) { @!a = @x; self }
    method setb(%x) { %!b = %x; self }
}

my $o = H.new;

for 1..3 {
    lives-ok { $o.seth({ a => A.new, b => A.new, c => B.new }) },
        "a hash of two role-doing classes is accepted (round $_)";
}
dies-ok { $o.seth({ a => A.new, b => A.new, c => C.new }) },
    'a later element of a class that does not do the role is rejected';
dies-ok { $o.seth({ a => C.new, b => A.new }) }, 'and so is an early one';
lives-ok { $o.seth({ a => SubA.new, b => A.new, c => SubA.new }) }, 'a subclass of a role-doing class';

lives-ok { $o.seta([A.new, A.new, B.new]) }, 'an array of role-doing instances';
dies-ok { $o.seta([A.new, A.new, C.new]) }, 'an array with an instance of another class';

lives-ok { $o.setb({ x => Kid.new, y => Kid.new, z => Base.new }) }, 'a class constraint takes the class and its subclass';
dies-ok { $o.setb({ x => Kid.new, y => A.new }) }, 'and refuses an unrelated class';

is $o.h.elems, 3, 'the last accepted hash is the one stored';
is $o.h.of.^name, 'Tr', 'the element type is recorded on the stored container';

try { $o.seth({ a => A.new, b => C.new }) };
like $!.message, /'%!h'|Type\s+check/, 'the failure names the type check';
