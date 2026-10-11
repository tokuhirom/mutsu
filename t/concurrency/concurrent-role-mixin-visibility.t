use Test;

# `$obj does R` on one thread reblesses the shared object into `C+{R}` in
# place; every other thread must then see that type with its roles and
# methods (#11456, ADR-11456).

plan 15;

role Ctx { has $.ctx is rw = 'fresh'; method hi { 'hi from Ctx' } }
role Extra { method extra { 'extra' } }
class Foo { method own { 'own' } }

sub mixed-elsewhere(&mix) {
    my $o = Foo.new;
    my $alias = $o;
    Thread.start({ mix($alias) }).finish;
    $o
}

my $f = mixed-elsewhere({ $_ does Ctx });
is $f.^name, 'Foo+{Ctx}', 'the object is reblessed for every alias';
ok $f ~~ Ctx, 'smartmatch on the main thread sees the role';
is $f.hi, 'hi from Ctx', 'a method of the mixed-in role dispatches';
is $f.ctx, 'fresh', 'the role attribute is seeded';
is $f.own, 'own', 'the base class method still dispatches';

is mixed-elsewhere({ $_ does Ctx }).^roles.map(*.^name).join(','), 'Ctx',
    '.^roles as the first query';
is mixed-elsewhere({ $_ does Ctx }).^mro.map(*.^name).join(','), 'Foo+{Ctx},Foo,Any,Mu',
    '.^mro as the first query';
ok mixed-elsewhere({ $_ does Ctx }).does(Ctx), '.does as the first query';
is mixed-elsewhere({ $_ does Ctx }).hi, 'hi from Ctx', 'a method call as the first query';

my $stacked = mixed-elsewhere({ $_ does Ctx; $_ does Extra });
is $stacked.^name, 'Foo+{Ctx}+{Extra}', 'two mixins stack';
is $stacked.extra, 'extra', 'the outer mixin type dispatches';
ok $stacked ~~ Ctx, 'the inner mixin type is adopted as well';

sub takes-ctx(Ctx $x) { $x.hi }
is takes-ctx(mixed-elsewhere({ $_ does Ctx })), 'hi from Ctx', 'a typed parameter binds it';

my $pooled = Foo.new;
await start { $pooled does Extra };
ok $pooled ~~ Extra, 'a mixin from a thread-pool task is visible after await';

my @objs = Foo.new xx 4;
await @objs.map(-> $o { start { $o does Ctx } });
ok @objs.map(* ~~ Ctx).all.so, 'concurrent mixins of one recipe are all visible';
