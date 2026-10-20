use Test;
use nqp;

# ADR-14162: a ClassHOW mixin's `create_BUILDPLAN` can add a `Code` task to
# the plan; construction runs it with the instance and the named arguments,
# after the attribute initializers and before TWEAK. `publish_method_cache`
# is called after the plan is created.

plan 7;

my @log;

sub with-extra-task(Mu \type, Mu $how is raw) {
    my \plan := nqp::getattr($how, Metamodel::ClassHOW, '@!BUILDPLAN');
    my $extra := my sub EXTRA(Mu $instance is raw, *%attrinit) {
        @log.push: type.^name ~ ':' ~ %attrinit.keys.sort.join(',');
        nqp::bindattr($instance, type, '$!seen', 'by-task');
    };
    my \newplan := nqp::create(nqp::what(plan));
    nqp::push(newplan, $extra);
    for ^nqp::elems(plan) -> $i {
        nqp::push(newplan, nqp::atpos(plan, $i));
    }
    nqp::bindattr($how, Metamodel::ClassHOW, '@!BUILDPLAN', newplan);
    {
        my \allplan := nqp::getattr($how, Metamodel::ClassHOW, '@!BUILDALLPLAN');
        my \newall := nqp::create(nqp::what(allplan));
        nqp::push(newall, $extra);
        for ^nqp::elems(allplan) -> $i {
            nqp::push(newall, nqp::atpos(allplan, $i));
        }
        nqp::bindattr($how, Metamodel::ClassHOW, '@!BUILDALLPLAN', newall);
    }
}

my role Hook {
    method create_BUILDPLAN(Mu \type) {
        callsame;
        @log.push: 'create';
        with-extra-task(type, self);
    }
    method publish_method_cache(Mu \type) {
        @log.push: 'publish';
        nextsame;
    }
}

class Plain {
    has $.seen = 'unset';
    has $.x = 1;
    submethod TWEAK { @log.push: 'tweak' }
    BEGIN { Plain.HOW does Hook }
}

is-deeply @log, ['create', 'publish'], 'both hooks ran at composition, in order';
@log = ();
my $o = Plain.new(:x(5));
is-deeply @log, ['Plain:x', 'tweak'], 'the extra task ran before TWEAK, with the named arguments';
is $o.seen, 'by-task', 'the task could set an attribute';
is $o.x, 5, 'the native initializers still ran';

class Child is Plain { has $.y = 2 }
@log = ();
my $c = Child.new(:y(7));
is-deeply @log, ['Plain:y', 'tweak'], 'a subclass runs the parent class task';
is $c.y, 7, 'subclass attribute';
is $c.seen, 'by-task', 'parent attribute set by the task';

done-testing;
