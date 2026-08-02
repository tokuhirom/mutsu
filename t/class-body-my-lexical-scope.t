use Test;

plan 9;

# A `my` (or `my constant`) declaration in a class body is a lexical of that
# body: its methods see it, and nothing outside does. Binding it under its bare
# name in the enclosing env instead made every class-body static a de facto
# global, so the next class body declaring the same name overwrote it and the
# FIRST class's methods started reading the SECOND class's value.
#
# Cro has four `my constant @defaults`, one per body-parser/serializer selector
# class, which is how a body-parser lookup ended up running the serializer list.

class CBML-A {
    my constant @defaults = <a b>;
    method get() { @defaults }
}
class CBML-B {
    my constant @defaults = <x y z>;
    method get() { @defaults }
}

is CBML-A.get.join(','), 'a,b', 'the first class keeps its own constant';
is CBML-B.get.join(','), 'x,y,z', 'and the second has its own';

class CBML-C {
    my $secret = 'c';
    method get() { $secret }
}
class CBML-D {
    my $secret = 'd';
    method get() { $secret }
}

is CBML-C.get, 'c', 'a plain `my` static is per-class too';
is CBML-D.get, 'd', 'and the later declaration does not steal it';

# The static stays mutable across calls, and the writeback is per class.
class CBML-Counter {
    my $n = 0;
    method bump() { $n++; $n }
}
CBML-Counter.bump;
is CBML-Counter.bump, 2, 'a class-body static keeps its value across calls';

# It does not escape the class body.
class CBML-Hidden { my $hidden = 'inside'; method get() { $hidden } }
is CBML-Hidden.get, 'inside', 'the method sees it';
nok $::('hidden').defined, 'and it is not bound in the enclosing scope';

# A method parameter of the same name shadows the static, as any inner `my`
# would; a caller lexical of the same name does not.
class CBML-Shadow {
    my $v = 'static';
    method with-param($v) { $v }
    method plain() { $v }
}
my $v = 'caller';
is CBML-Shadow.with-param('param'), 'param', 'a parameter shadows the static';
is CBML-Shadow.plain, 'static', 'a same-named caller lexical does not';

done-testing;
