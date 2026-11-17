use Test;

# `my $x = <Array / Hash / object>` takes the lean declaration lane (#15737):
# the value gets the `$` container's itemization, and the variable holds the
# same object as its source, exactly as the general store gives it. Each
# shape runs in a routine called often enough for the lane (and the JIT) to
# be on the hot path.

plan 13;

my %memo;
%memo<a> = [1, 2];
sub from-hash() { my $k = %memo<a>; $k }
my $k;
$k = from-hash() for ^50;
is-deeply $k, $[1, 2], 'an Array read from a hash, declared into a $ scalar';
ok $k.VAR.^name eq 'Scalar', '... the declaration is a Scalar container';
{
    my $j = from-hash();
    $j[0] = 9;
    is-deeply %memo<a>, [9, 2], '... holding the same Array as the hash';
}

sub decl-array(@a) { my $x = @a; $x }
my @src = 1, 2;
my $x;
$x = decl-array(@src) for ^50;
is $x.raku, '$[1, 2]', 'a bare Array is itemized by the declaration';
sub push-through(@a) { my $x = @a; $x.push(3); @a }
is-deeply push-through([1, 2]), [1, 2, 3], '... and shares the Array';

sub decl-hash(%h) { my $h = %h; $h }
my $h;
$h = decl-hash({ a => 1 }) for ^50;
is $h.raku, '${:a(1)}', 'a Hash is itemized by the declaration';

sub decl-list() { my $l = (1, 2, 3); $l.elems }
my $n;
$n = decl-list() for ^50;
is $n, 3, 'a List in a $ scalar is one item';
sub flat-count() { my $l = (1, 2, 3); my $c = 0; $c++ for $l; $c }
my $c;
$c = flat-count() for ^50;
is $c, 1, '... iterating it gives one item';

class P { has $.v }
sub decl-obj($v) { my $o = P.new(:$v); $o.v }
my $ov;
$ov = decl-obj($_) for ^50;
is $ov, 49, 'an object instance';

sub decl-nil() { my $z = Nil; $z }
my $z = 1;
$z = decl-nil() for ^50;
is $z.raku, 'Any', 'Nil still resets the declaration to Any';

sub decl-typed() { my Array $t = [1]; $t }
my $t;
$t = decl-typed() for ^50;
is-deeply $t, $[1], 'a typed declaration keeps its value';
sub decl-typed-bad() { my Int $t = [1]; $t }
throws-like { decl-typed-bad() }, X::TypeCheck::Assignment,
    'a mismatched typed declaration still dies';

sub closure-capture() {
    my $cap = [1];
    my &f = { $cap.push(2) };
    f();
    $cap
}
my $cc;
$cc = closure-capture() for ^50;
is-deeply $cc, $[1, 2], 'a captured declaration sees the closure write';
