use Test;
# A method of a parametric role called on a `pun but Role` type object that
# another thread made keeps `self` bound to the mixin type (not its bare base
# pun), so `self.new` builds an instance that does the mixed-in role. Identical
# under rakudo.
plan 7;

role R[::T = Any] { has $.v; method mk(\v) { self.new(:v(v)) }; method me { self.^name } }
role M2 { }

my $U;
my $go = Promise.new;
my @seen;
my $b = Thread.start({
    await $go;
    my $p = $U.mk(1);
    @seen.push: ($p.^name, so $p ~~ M2, $U.me);
});
Thread.start({ $U = R.^parameterize(Int).^pun but M2; $go.keep }).finish;
$b.finish;

is @seen[0][0], 'R[Int]+{M2}', 'a thread that started before the type existed: instance class';
ok @seen[0][1], 'and the instance does the mixed-in role';
is @seen[0][2], 'R[Int]+{M2}', 'self inside a role method is the mixin type';

my $p = $U.mk(2);
is $p.^name, 'R[Int]+{M2}', 'main thread: instance class';
ok $p ~~ M2, 'main thread: instance does the role';
is $p.v, 2, 'main thread: attribute set';
my $q = await start { $U.mk(3) };
is $q.^name, 'R[Int]+{M2}', 'a thread started afterwards agrees';
