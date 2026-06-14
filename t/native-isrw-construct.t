use v6;
use Test;

# VM-native default construction now covers `is rw` attributes (Track A ③
# constructor). Previously the native builder mis-read the tuple's `is_rw`
# field as "required" and fell back whenever an `is rw` attribute was not
# passed to .new; an `is rw` attribute just gets its normal default, so it
# stays native. The actual `is required` flag still falls through correctly.

plan 15;

# --- read / write / default of an is-rw attribute ---
class A { has $.x is rw; }
my $a = A.new(x => 5);
is $a.x, 5, 'is rw attribute reads a provided value';
$a.x = 9;
is $a.x, 9, 'is rw attribute is writable through its accessor';
my $a2 = A.new;
nok $a2.x.defined, 'an unprovided is rw attribute defaults to undefined (no fallback)';
$a2.x = 7;
is $a2.x, 7, 'an unprovided is rw attribute is still writable';

# --- is rw with a default ---
class B { has $.n is rw = 10; }
is B.new.n, 10, 'is rw attribute uses its default';
my $b = B.new;
$b.n = 20;
is $b.n, 20, 'is rw defaulted attribute is writable';

# --- is rw + BUILD ---
class C { has $.v is rw; submethod BUILD(:$v = 1) { $!v = $v } }
my $c = C.new(v => 3);
is $c.v, 3, 'is rw + BUILD constructs natively';
$c.v = 99;
is $c.v, 99, 'is rw + BUILD attribute is writable';

# --- is rw + TWEAK ---
class D { has $.a is rw; has $.b is rw; submethod TWEAK { $!b = ($!a // 0) + 1 } }
my $d = D.new(a => 5);
is $d.b, 6, 'is rw + TWEAK derives an attribute';
$d.b = 100;
is $d.b, 100, 'is rw + TWEAK attribute is writable';

# --- typed is rw ---
class E { has Int $.i is rw; }
my $e = E.new(i => 42);
is $e.i, 42, 'typed is rw attribute reads a value';
$e.i = 7;
is $e.i, 7, 'typed is rw attribute is writable';

# --- class-level `is rw` ---
class F is rw { has $.p; has $.q; }
my $f = F.new(p => 1, q => 2);
$f.p = 10;
is "{$f.p} {$f.q}", "10 2", 'class-level is rw makes all attributes writable';

# --- a genuine `is required` attribute is still enforced (falls through) ---
class R { has $.r is required; }
nok (try R.new).defined, 'an is required attribute is still enforced';
class RH { has @.h is required; }
nok (try RH.new).defined, 'an is required @ attribute is still enforced';
