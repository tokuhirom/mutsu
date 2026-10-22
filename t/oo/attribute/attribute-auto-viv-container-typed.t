use Test;
use nqp;

plan 13;

# `Attribute.auto_viv_container` of a typed `@`/`%` attribute is an empty typed
# container, and `nqp::clone_nd` of a typed hash keeps its type (#14266,
# AttrX::Mooish clones the container to build an attribute's value).
class C {
    has Int %.h;
    has Int @.a;
    has %.plain;
    has @.plain-a;
}
my ($h, $a, $plain, $plain-a) = C.^attributes;

is $h.auto_viv_container.WHAT.raku, 'Hash[Int]', 'typed hash attribute';
is $a.auto_viv_container.WHAT.raku, 'Array[Int]', 'typed array attribute';
is $plain.auto_viv_container.WHAT.raku, 'Hash', 'untyped hash attribute';
is $plain-a.auto_viv_container.WHAT.raku, 'Array', 'untyped array attribute';

my Mu $aviv := $h.auto_viv_container;
my $clone = nqp::clone_nd($aviv);
is $clone.WHAT.raku, 'Hash[Int]', 'nqp::clone_nd keeps the hash type';
$clone.STORE(%(a => 1));
is-deeply $clone, (my Int % = a => 1), 'STORE into the clone';
is $aviv.elems, 0, 'the original stays empty';
my Int %typed = a => 1;
is nqp::clone_nd(%typed).WHAT.raku, 'Hash[Int]', 'cloning a typed hash variable';

# A definite element type keeps its smiley (#14389).
class D { has Int:D %.h; has Str:D @.a; }
my ($dh, $da) = D.^attributes;
is $dh.type.raku, 'Associative[Int:D]', 'Attribute.type of Int:D %';
is $da.type.raku, 'Positional[Str:D]', 'Attribute.type of Str:D @';
is $dh.auto_viv_container.WHAT.raku, 'Hash[Int:D]', 'auto_viv_container of Int:D %';
is $da.auto_viv_container.WHAT.raku, 'Array[Str:D]', 'auto_viv_container of Str:D @';
is D.new.h.WHAT.raku, 'Hash[Int:D]', 'the constructor agrees';
