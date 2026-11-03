use Test;

# `has Inp() $.x .= new` calls `new` on the coercion's target type (#15845);
# the spelled `Inp()` used to reach the RakuAST frontend as a bareword.

plan 5;

class Inp { has $.v = 3 }
class Holder {
    has Inp() $.a .= new;
    has Inp() $.b .= new(v => 9);
    has Inp() $.c handles<v> .= new(v => 4);
}

my $h = Holder.new;
is $h.a.^name, 'Inp', 'a coercion-typed attribute is initialised by .= new';
is $h.a.v, 3, 'with the default arguments';
is $h.b.v, 9, 'with arguments';
is $h.v, 4, 'together with a handles trait';

class Conv { has Int(Str) $.n .= new }
is Conv.new.n, 0, 'a coercion with a source type calls new on the target';
