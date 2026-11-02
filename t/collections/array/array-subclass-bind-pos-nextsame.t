use Test;

# #15989: a user `BIND-POS` on an `is Array` subclass that defers with
# `nextsame` / `callsame` / `nextwith` reaches the native binding of the
# backing storage. The expected values are rakudo's.

plan 11;

class W is Array { method BIND-POS($i, \v) { nextsame } }
my @w is W = <a b c>;
is @w.BIND-POS(1, "x"), "x", 'nextsame answers the bound value';
is @w.join(","), "a,x,c", 'and the element is bound';

class V is Array { }
my @v is V = <a b c>;
is @v.BIND-POS(1, "x"), "x", 'without an override';
is @v.join(","), "a,x,c", 'the element is bound';

class A is Array { method BIND-POS($i, \v) { callsame } }
my @a is A = <a b c>;
is @a.BIND-POS(0, "p"), "p", 'callsame answers the bound value';
is @a.join(","), "p,b,c", 'and binds it';

class B is Array { method BIND-POS($i, \v) { nextwith $i, v.uc } }
my @b is B = <a b c>;
is @b.BIND-POS(2, "q"), "Q", 'nextwith passes its own arguments';
is @b.join(","), "a,b,Q", 'and binds them';

class R is Array { method BIND-POS($i, \v) { nextwith $i.round, v } }
my @r is R = <a b c>;
@r[1.4] := "Y";
is @r.join(","), "a,Y,c", 'a subscript bind goes through the override';

my class Count is Array {
    has $.calls = 0;
    method BIND-POS($i, \v) { $!calls++; nextsame }
}
my @c is Count = <a b c>;
@c[0] := "m";
@c[2] := "n";
is @c.calls, 2, 'the override ran for each bind';
is @c.join(","), "m,b,n", 'and both binds took';
