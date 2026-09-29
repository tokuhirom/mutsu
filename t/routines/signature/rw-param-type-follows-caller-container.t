use Test;

# An `is rw` / `is raw` parameter aliases the CALLER's container: its declared
# type is checked when the argument binds, and every later write is checked
# against the caller container's own constraint, not the parameter's (#10146).

plan 16;

sub typed-rw(Str:D $s is rw) { $s = 5 }
my $z = "a";
lives-ok { typed-rw($z) }, 'typed rw param accepts a write of another type';
isa-ok $z, Int, '... which lands in the untyped caller container';

sub typed-raw(Str $s is raw) { $s = 5 }
my $r = "a";
typed-raw($r);
isa-ok $r, Int, 'typed raw param writes through to the untyped caller';

sub typed-rw-elem(Str $s is rw) { $s = 5 }
my @a = "a";
typed-rw-elem(@a[0]);
isa-ok @a[0], Int, 'typed rw param writes into an untyped array element';
my %h = k => "v";
typed-rw-elem(%h<k>);
isa-ok %h<k>, Int, 'typed rw param writes into an untyped hash value';

dies-ok { typed-rw(5) }, 'the declared type is still checked at bind time';

my Str $t = "a";
throws-like { typed-rw-elem($t) }, X::TypeCheck::Assignment,
    'typed rw param: a typed caller container still rejects the write';
is $t, "a", '... and keeps its value';

sub untyped-rw($s is rw) { $s = 7 }
my Str $u = "a";
throws-like { untyped-rw($u) }, X::TypeCheck::Assignment,
    'untyped rw param: the caller container constraint applies';
is $u, "a", '... and the caller keeps its value';

# Through the positional-light call path (a plain statement-level call).
my Str $w = "a";
my $died = False;
{ untyped-rw($w); CATCH { default { $died = True } } }
ok $died, 'untyped rw param from a direct call: the caller constraint applies';

my $v = "a";
untyped-rw($v);
is $v, 7, 'untyped rw param into an untyped caller still writes';

# String::Fields' `apply-fields` shape: assign an object into a `Str:D is rw`.
class Wrapper { has $.s }
sub wrap(Str:D $string is rw) { $string = Wrapper.new(s => $string) }
my $foo = "text";
wrap($foo);
is $foo.s, "text", 'a Str:D rw param can be replaced by a non-Str object';

# `$x.&f(...)` is `f($x, ...)`: the invocant binds as its container, so an
# `is rw` first parameter writes back and a multi picks the rw candidate.
proto sub apply(|) {*}
multi sub apply(Str:D $s is rw, Int:D $n) { $s = $n }
multi sub apply(Str:D $s, Int:D $n) { 'ro' }
my $m = "x";
$m.&apply(3);
isa-ok $m, Int, '.& call reaches the rw multi candidate and writes back';

sub set-it(Str:D $s is rw, $v) { $s = $v }
my $o = "x";
$o.&set-it(Wrapper.new(s => "w"));
is $o.s, "w", '.& call on an rw only sub writes back';

sub bump($x is rw) { $x++ }
my @b = 1, 2;
@b[0].&bump;
is-deeply @b, [2, 2], '.& call with an element invocant writes into the array';
