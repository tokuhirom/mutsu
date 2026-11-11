use Test;

# A method's sigilless (`\x`) parameter given a value (a literal, `self`, a
# call result, a type object) is a read-only term; given a variable it aliases
# the caller's container. Each case runs twice so the call goes through the
# cached dispatch too. Found via AttrX::Mooish, whose methods take `Mu \obj`
# and are called with `self` (#17335).

plan 9;

class A {
    method echo(Mu \i) { i }
    method set(\i, $v) { i = $v }
    method double(Int \i) { i * 2 }
    method me { self.echo(self) === self }
    method two(Mu \a, Mu \b) { a.^name ~ '/' ~ b.^name }
}

my $a = A.new;
is (^2).map({ $a.echo(3) }).join(','), '3,3', 'a literal binds';
is (^2).map({ $a.me }).join(','), 'True,True', '`self` binds as itself';
is (^2).map({ $a.echo(Int).^name }).join(','), 'Int,Int', 'a type object binds';

my @r;
for ^2 { @r.push: (try { $a.set(3, 4); 'ok' }) // $!.^name }
is @r.join(','), 'X::Assignment::RO,X::Assignment::RO', 'a literal argument is read-only';

my $v = 1;
$a.set($v, 5) for ^2;
is $v, 5, 'a variable argument is still aliased';

my \k = 9;
@r = ();
for ^2 { @r.push: (try { $a.set(k, 1); 'ok' }) // 'died' }
is @r.join(','), 'died,died', 'a read-only sigilless caller term stays read-only';

is (^2).map({ $a.double(21) }).join(','), '42,42', 'a typed sigilless parameter binds';
@r = ();
my $s = 'x';
for ^2 { @r.push: (try { $a.double($s) }) // $!.^name }
is @r.join(','), 'X::TypeCheck::Binding::Parameter,X::TypeCheck::Binding::Parameter',
    '... and checks its constraint';

is (^2).map({ $a.two(1, 'b') }).join(','), 'Int/Str,Int/Str', 'two sigilless parameters';
