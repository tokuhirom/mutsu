use Test;

plan 27;

# `$!x = <scalar>` takes a direct store lane once a store site is warm. Every
# case stores several times so the first (general) and later (lane) stores are
# both exercised, and the observable results must be identical.

class Plain {
    has $.a;
    has Int $.i;
    has Str $.s;
    has Num $.n;
    has Bool $.b;
    has int $.ni;
    has str $.ns;
    has num $.nn;
    method set-a($v)  { $!a  = $v; self }
    method set-i($v)  { $!i  = $v; self }
    method set-s($v)  { $!s  = $v; self }
    method set-n($v)  { $!n  = $v; self }
    method set-b($v)  { $!b  = $v; self }
    method set-ni($v) { $!ni = $v; self }
    method set-ns($v) { $!ns = $v; self }
    method set-nn($v) { $!nn = $v; self }
    method get-a { $!a }
}

my $p = Plain.new;
for 1..4 -> $k {
    $p.set-a($k);
    $p.set-i($k);
    $p.set-s("s$k");
    $p.set-n($k * 1e0);
    $p.set-b($k %% 2);
    $p.set-ni($k);
    $p.set-ns("n$k");
    $p.set-nn($k * 2e0);
}
is $p.a, 4, 'untyped attribute holds the last store';
is $p.i, 4, 'Int attribute';
is $p.s, 's4', 'Str attribute';
is $p.n, 4e0, 'Num attribute';
is $p.b, True, 'Bool attribute';
is $p.ni, 4, 'int attribute';
is $p.ns, 'n4', 'str attribute';
is $p.nn, 8e0, 'num attribute';
is $p.get-a, 4, 'a read inside a method sees the store';

# A value that does not satisfy the declaration still fails after the site is warm.
for 1..3 { $p.set-i(7) }
throws-like { $p.set-i("seven") }, X::TypeCheck::Assignment, 'a Str into an Int attribute dies when warm';
is $p.i, 7, 'the failed store left the attribute alone';
for 1..3 { $p.set-ns("x") }
dies-ok { $p.set-ns(5) }, 'an Int into a str attribute dies when warm';

# An undefined store resets a typed attribute to its type object, an untyped one to Any.
$p.set-i(Nil);
ok $p.i === Int, 'Nil resets an Int attribute to Int';
$p.set-a(Nil);
ok $p.a === Any, 'Nil resets an untyped attribute to Any';

# Two instances of one class do not share the store.
my $q = Plain.new;
$q.set-a(1) for ^3;
$p.set-a(2) for ^3;
is $q.a, 1, 'the other instance keeps its own value';
is $p.a, 2, 'this instance keeps its own value';

# A subclass has its own layout; the site is shared by the inherited method.
class Sub is Plain { has $.extra; method set-extra($v) { $!extra = $v; self } }
my $sub = Sub.new;
$sub.set-a($_) for 1..3;
$sub.set-extra($_) for 1..3;
is $sub.a, 3, 'inherited method stores into the subclass instance';
is $sub.extra, 3, 'subclass attribute';
$p.set-a($_) for 10..12;
is $p.a, 12, 'the parent class instance is unaffected by the subclass layout';

# A `where` clause is still enforced.
class Checked {
    has Int $.n where * > 0;
    method set($v) { $!n = $v; self }
}
my $c = Checked.new(n => 1);
$c.set($_) for 1..3;
is $c.n, 3, 'where-constrained attribute accepts a passing value';
dies-ok { $c.set(-1) }, 'where-constrained attribute still rejects a failing value when warm';

# A bound attribute writes through its container.
class Bindy {
    has $.x;
    method bind-to($cell is raw) { $!x := $cell; self }
    method set($v) { $!x = $v; self }
}
my $cell = 0;
my $bd = Bindy.new;
$bd.set(1) for ^3;
$bd.bind-to($cell);
$bd.set(42);
is $cell, 42, 'a store into a bound attribute writes the bound container';

# A role method storing into a class attribute.
role Setter { has $.r; method set-r($v) { $!r = $v; self } }
class WithRole does Setter { }
my $wr = WithRole.new;
$wr.set-r($_) for 1..3;
is $wr.r, 3, 'role method stores into the composing class attribute';

# A closure created in the method sees the stored value.
class Clo {
    has $.v;
    method run($x) { $!v = $x; my &f = { $!v }; f() }
}
my $cl = Clo.new;
my @seen = (1..4).map({ $cl.run($_) });
is-deeply @seen, [1, 2, 3, 4], 'a closure reads the attribute just stored';

# The private store is visible through the public accessor immediately.
class Acc {
    has $.val;
    method bump { $!val = ($!val // 0) + 1; $.val }
}
my $acc = Acc.new;
is-deeply (^4).map({ $acc.bump }).List, (1, 2, 3, 4), 'accessor sees every store';

# A store in BUILD / TWEAK counts as set (no default overwrite afterwards).
class Tw {
    has $.k = 'default';
    submethod TWEAK { $!k = 'tweaked' }
}
is Tw.new.k, 'tweaked', 'TWEAK store survives the default pass';
is Tw.new.k, 'tweaked', 'TWEAK store survives the default pass (warm)';
