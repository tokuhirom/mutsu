use Test;

plan 4;

# Gauge (ecosystem): `role Iterator does Iterator` inside `unit class Gauge`
# composes the core Iterator, it is not a role composing itself.
class Gauge {
    role Iterator does Iterator {
        method is-lazy(--> True) { }
    }
    class It does Iterator {
        method pull-one { IterationEnd }
    }
    method lazy { It.new.is-lazy }
}

is Gauge.lazy, True, 'a class composing the nested role gets its method';
ok Gauge::It.new ~~ Iterator, 'and is still a core Iterator';
ok Gauge::It.new ~~ Gauge::Iterator, 'it does the nested role';

throws-like { EVAL 'role Foo does Foo { }' }, Exception, 'a role with no core namesake cannot compose itself';
