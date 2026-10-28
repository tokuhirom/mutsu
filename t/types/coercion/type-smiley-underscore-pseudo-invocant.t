use Test;

plan 2;

# Gauge (ecosystem): an invocant `::?CLASS:_:` must survive the RakuAST round
# trip (the frontend refused the `:_` smiley on a pseudo-type capture).
class A {
    method m(::?CLASS:_: $x) { $x }
}
is A.new.m(3), 3, '::?CLASS:_: invocant on an instance';
is A.m(4), 4, 'and on the type object';
