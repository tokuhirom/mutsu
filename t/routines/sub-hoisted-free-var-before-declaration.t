use Test;

# A named sub called textually BEFORE its own declaration still reads the
# free variables visible at that declaration, not a same-named `my` of its
# caller (mutsu#9911; ADR-0024's "textual-order edge").

plan 14;

my $c = 'outer';
{ my $c = 'inner'; is f(), 'outer', 'read from a shadowing block before the sub' }
sub f { $c }

is early(), Any, 'a call before the declaration has run reads Any';
my $e = 'set';
is early(), 'set', 'and the declared value once it has';
sub early { $e }

my $w = 1;
{ my $w = 100; set-w(5); is $w, 100, 'write leaves the shadowing local alone' }
is $w, 5, 'write reaches the declaration-site variable';
sub set-w($v) { $w = $v }

my $u;
{ my $u = 'in'; is u-val(), 'undef', 'uninitialized declaring lexical' }
$u = 'later';
{ my $u = 'in2'; is u-val(), 'later', 'later assignment is seen' }
sub u-val { $u // 'undef' }

class Foo { has $.x }
my Foo $o = Foo.new(x => 1);
{ my Foo $o = Foo.new(x => 2); is o-val().x, 1, 'type-constrained variable' }
sub o-val { $o }

{
    my $b = 'blk';
    { my $b = 'shadow'; is b-val(), 'blk', 'sub declared in a bare block' }
    sub b-val { $b }
}

my @r;
for 1..2 -> $i {
    my $z = "z$i";
    { my $z = 'no'; @r.push: z-val() }
    sub z-val { $z }
}
is @r.join(','), 'z1,z2', 'each loop iteration binds a fresh variable';

my @a = 1, 2;
{ my @a = 9; is a-val(), [1, 2], 'array free variable' }
sub a-val { @a }

my %h = a => 1;
{ my %h = b => 2; is h-val(), {a => 1}, 'hash free variable' }
sub h-val { %h }

my $n = 0;
{ my $n = 50; bump(); bump(); is $n, 50, 'shadowing local untouched by increments' }
is $n, 2, 'increments reach the declaration-site variable';
sub bump { $n++ }
