use Test;
plan 8;

class C {
    has @!a;
    method push(*@v) { @!a.push(|@v); self }
    method vals { @!a }
    method viv0(C:U $self is rw:) { $self = C.new; 1 }
    method viv(C:U $self is rw:) { $self = C.new; $self.push(7); 1 }
    method viv3(C:U $self is rw:) { $self = C.new; $self }
    method swap(C:D $self is rw:) { $self = C.new; 1 }
}

my C $b;
$b.viv0;
ok $b.defined, 'an explicit `$self is rw` invocant assigned in the body reaches the caller';
$b.push(3);
is $b.vals, [3], 'and the new object is the caller\'s';

my C $a;
$a.viv;
is $a.vals, [7], 'push on the reassigned $self reaches the caller object';

my C $d;
my $r = $d.viv3;
ok $r === $d, 'returning the reassigned $self returns the caller object';

my $x = C.new;
my $y = $x;
$x.swap;
ok $x !=== $y, 'a defined invocant is replaced too';
ok $y.defined, 'the old object is untouched';

# `self` keeps the original object.
class D {
    method who(D:U $self is rw:) { $self = D.new; (self.defined, $self.defined) }
}
my D $z;
is $z.who, (False, True), '`self` still names the original invocant';
ok $z.defined, 'while the caller variable was assigned';
