use Test;

plan 2;

# Gauge (ecosystem): `It($x)` in a method of `class Gauge` invokes the CALL-ME
# of the nested `Gauge::It`; the call-position lookup only saw GLOBAL.
class Gauge {
    class It {
        has $.b;
        method CALL-ME($b) { self.bless: :$b }
    }
    method via-method($x) { It($x) }
    sub via-sub($x) { It($x) }
    method via-sub($x) { via-sub($x) }
}

is Gauge.via-method(5).b, 5, 'in a method';
is Gauge.via-sub(6).b, 6, 'in a sub of the class';
