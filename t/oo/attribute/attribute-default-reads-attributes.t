use Test;

# An attribute default sees the object's other attributes when it names them
# (`$!x`, `$.x`, in an expression, a closure or an interpolation), and a
# default that names none (`Lock.new`, a type object, `now`) still gets its
# own value. mutsu binds the attributes only for the first kind.

plan 11;

class C {
    has $.x = 2;
    has $.y = $!x * 10;
    has &.cb = { $!x + 1 };
    has Str $.s = "x=$!x";
    has @.list = $!x xx 2;
    has $.lock = Lock.new;
    has Str $.typed;
    has Instant $.t = now;
    has $.e = EVAL('3');
}

my $c = C.new;
is $c.y, 20, 'a default reads an earlier attribute';
is $c.cb.(), 3, 'a closure default captures the attributes';
is $c.s, 'x=2', 'an interpolating default reads an attribute';
is-deeply $c.list, [2, 2], 'a list default reads an attribute';
isa-ok $c.lock, Lock, 'a default that names no attribute still runs';
is $c.typed, Str, 'an attribute with no default holds its type object';
isa-ok $c.t, Instant, 'now as a default';
is $c.e, 3, 'EVAL in a default';

my $d = C.new(x => 5);
is $d.y, 50, 'the default sees a constructor-supplied attribute';
is $d.cb.(), 6, 'and so does the closure';
isnt $c.lock, $d.lock, 'each object gets its own Lock';
