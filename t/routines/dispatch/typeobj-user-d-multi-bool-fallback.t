use Test;
plan 5;

role R {
    has @.a;
    multi method Bool(::?CLASS:D:) { so @!a }
}
class C does R { }

my C $c;
is $c.Bool, False, 'type object falls back to Mu.Bool when only a :D candidate exists';
is ?$c, False, 'prefix ? on the type object';
is C.new.Bool, False, 'empty instance uses the :D candidate';
is C.new(a => [1]).Bool, True, 'non-empty instance uses the :D candidate';

class G { }
class H is G { }
multi sub infix:<eqv>(G \a, G \b --> Bool:D) { True }
ok H eqv H.new, 'user eqv candidate takes a type object and an instance';
