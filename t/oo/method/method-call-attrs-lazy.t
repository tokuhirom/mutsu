use Test;

# A compiled method call hands the receiver's live attribute cell to the
# binder and snapshots it only when the full binder runs (#8880). Each shape
# below reads or writes attributes through a different binder path.

plan 9;

# Fast path: attribute reads and writes on repeated calls.
class Counter {
    has $.n = 0;
    method bump() { $!n++; $!n }
}
my $c = Counter.new;
$c.bump for ^5;
is $c.bump, 6, 'fast path sees every earlier attribute write';

# Full path (an `is rw` parameter): attributes read from the snapshot.
class Copier {
    has $.v = 10;
    method into($x is rw) { $x = $!v; $!v++ }
}
my $cp = Copier.new;
my $dst;
$cp.into($dst);
$cp.into($dst);
is $dst, 11, 'full path binds the current attribute values';
is $cp.v, 12, 'full path attribute writes persist';

# A role mixed into an instance: the role cell first, the instance cell as
# the fallback.
role Tagged {
    has $.tag = 'r';
    method both() { $.tag ~ ':' ~ self.base-name }
}
class Base { has $.base-name = 'b'; }
my $t = Base.new but Tagged;
is $t.both, 'r:b', 'role method sees role and instance attributes';
is $t.both, 'r:b', 'and again on a second call';

# A sigilless attribute alias keeps the full path.
class Sigilless {
    has $x;
    submethod BUILD(:$!x = 3) { }
    method get() { $x }
}
is Sigilless.new.get, 3, 'sigilless attribute read through its alias';

# `:=` binding an attribute inside a method survives the method exit.
class Binder {
    has $.slot;
    method bind-to($c is rw) { $!slot := $c }
}
my $outer = 1;
my $b = Binder.new;
$b.bind-to($outer);
$outer = 42;
is $b.slot, 42, ':= attribute binding is kept past the method exit';

# A deferral candidate (callsame) runs with the receiver's attributes.
class P { has $.p = 'p'; method who() { 'P' ~ $!p } }
class Q is P { method who() { 'Q' ~ callsame } }
is Q.new.who, 'QPp', 'callsame candidate reads the receiver attributes';
is Q.new(p => 'z').who, 'QPz', 'with a constructed attribute value';
