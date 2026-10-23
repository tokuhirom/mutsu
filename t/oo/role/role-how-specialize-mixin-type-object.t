use Test;
use nqp;

# A role attribute trait that mixes a meta-role into the role's HOW
# (`$*PACKAGE.HOW does RHow`) reaches `Class.^mixin(R)` and `(Class.^mixin(R)).new`
# like it reaches `class C does R` (#13264, AttrX::Mooish `t/020-role`).

plan 8;

my @built;

role Helper {
    method setup-it(Mu \type) { "setup {type.^name}" }
    method create_BUILDPLAN(Mu \type) is raw {
        callsame;
        my \plan := nqp::getattr(self, Metamodel::ClassHOW, '@!BUILDPLAN');
        my \newplan := nqp::create(nqp::what(plan));
        nqp::push(newplan, my sub TASK(Mu $instance is raw, *%) { @built.push(type.^name) });
        while nqp::elems(plan) { nqp::push(newplan, nqp::shift(plan)) }
        nqp::bindattr(self, Metamodel::ClassHOW, '@!BUILDPLAN', newplan);
        # BUILDALLPLAN is what `.new` runs; a "phony required" task (code 1000)
        # is not valid in it.
        my \allplan := nqp::create(nqp::what(plan));
        my int $i = 0;
        while $i < nqp::elems(newplan) {
            my $task := nqp::atpos(newplan, $i);
            nqp::push(allplan, $task)
                unless nqp::islist($task) && nqp::hllize(nqp::atpos($task, 0)) == 1000;
            ++$i;
        }
        nqp::bindattr(self, Metamodel::ClassHOW, '@!BUILDALLPLAN', allplan);
    }
}

role RHow {
    method specialize(Mu \r, Mu:U \obj, *@, *%) {
        obj.HOW does Helper unless obj.HOW ~~ Helper;
        nextsame;
    }
}

multi trait_mod:<is>(Attribute:D $a, :$tr!) {
    given $*PACKAGE.HOW {
        $_ does RHow unless $_ ~~ RHow;
    }
}

role Named { has $!q is tr; }

my class Foo { }
my $named := Foo.^mixin: Named;
is $named.^setup-it, 'setup Foo+{Named}', 'a named role: the mixin class HOW got the role HOW hook';
ok $named.HOW ~~ Helper, '.HOW of the mixin type object is the one the hook mixed into';

my class Bar { }
my $anon := Bar.^mixin: role { has $!z is tr };
is $anon.^setup-it, 'setup Bar+{<anon|1>}', 'an anonymous role: the mixin class HOW got the hook';

@built = ();
$named.new;
is-deeply @built.List, ('Foo+{Named}',), '.new on a mixin type object runs the mixin class plan';

@built = ();
$anon.new;
is-deeply @built.List, ('Bar+{<anon|1>}',), 'also for an anonymous role';

my class Baz does Named { }
is Baz.^setup-it, 'setup Baz', 'class does R: the hook still applies';
@built = ();
Baz.new;
is-deeply @built.List, ('Baz',), 'class does R: the plan runs';

my class Qux { }
my $plain := Qux.^mixin: role { method hi { 'hi' } };
is $plain.new.hi, 'hi', 'a role without hooks still composes through .new';
