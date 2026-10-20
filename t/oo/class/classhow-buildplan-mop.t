use Test;
use nqp;

# A class's BUILDPLAN is visible and replaceable through the meta-object
# protocol, in Rakudo's encoding (ADR-14162, #14162): AttrX::Mooish's HOW
# mixin overrides `create_BUILDPLAN`, rewrites `@!BUILDPLAN` and binds it.

plan 13;

sub codes(Mu \how, Str $attr) {
    my \plan := nqp::getattr(how, Metamodel::ClassHOW, $attr);
    (^nqp::elems(plan)).map({
        my $t := nqp::atpos(plan, $_);
        nqp::islist($t)
            ?? nqp::atpos($t, 0) ~ ':' ~ nqp::atpos($t, 2)
            !! ($t.name || 'CODE')
    }).join(' ')
}

class A {
    has $.a = 1;
    has $.b is required;
    has $!c;
    has Int $.d;
    has $.o is default(9);
    has int $.i = 3;
    submethod TWEAK { }
}

is codes(A.HOW, '@!BUILDPLAN'),
    '0:$!a 0:$!b 0:$!d 0:$!o 1:$!i 800:$!b 400:$!a 401:$!i 1000:$!c 1000:$!d 1000:$!o TWEAK',
    'the BUILDPLAN of a class, task by task';

class P { has $.p = 5 }
class B is P { has $.q; submethod BUILD(:$!q = 2) { } }
is codes(B.HOW, '@!BUILDPLAN'), 'BUILD 1000:$!q',
    'a class with a BUILD has no init tasks of its own';
is codes(B.HOW, '@!BUILDALLPLAN'), '0:$!p 400:$!p BUILD',
    'the BUILDALLPLAN runs the MRO least derived first, without no-ops';

is B.^BUILDPLAN.map({ $_ ~~ Code ?? .name !! .[0] }).join(','), 'BUILD,1000',
    '.^BUILDPLAN is a list of tasks';
is A.^attributes.first(*.name eq '$!o').build.^name, 'Mu',
    'an is default value alone is no build value';

role Mooify {
    method create_BUILDPLAN(Mu \type) is raw {
        callsame;
        my \plan := nqp::getattr(self, Metamodel::ClassHOW, '@!BUILDPLAN');
        my \newplan := nqp::create(nqp::what(plan));
        while nqp::elems(plan) {
            my $task := nqp::shift(plan);
            if nqp::islist($task) && nqp::atpos($task, 2) eq '$!a' {
                nqp::push(newplan, sub ($inst, *%init) {
                    nqp::bindattr(nqp::decont($inst), type, '$!a', (%init<a> // 0) * 10)
                }) if nqp::atpos($task, 0) == 0;
            }
            else {
                nqp::push(newplan, $task);
            }
        }
        nqp::bindattr(self, Metamodel::ClassHOW, '@!BUILDPLAN', newplan);
        nqp::bindattr(self, Metamodel::ClassHOW, '@!BUILDALLPLAN', newplan);
    }
}
my @tweaked;
multi trait_mod:<is>(Attribute:D $attr, :$mooify!) {
    $*PACKAGE.HOW does Mooify unless $*PACKAGE.HOW ~~ Mooify;
}
class M {
    has $.a is mooify = 1;
    has $.b is required;
    has Int $.d = 4;
    submethod TWEAK { @tweaked.push: $!a }
}

my $m = M.new(a => 2, b => 3);
is "{$m.a} {$m.b} {$m.d}", '20 3 4', 'a replaced plan runs, its Code task included';
is @tweaked.join(','), '20', 'TWEAK runs after the Code task';
is M.new(b => 1).a, 0, 'the Code task sees the missing named argument';
throws-like { M.new(a => 1) }, X::Attribute::Required, 'the required check still runs';
is codes(M.HOW, '@!BUILDPLAN').split(' ').head(2).join(' '),
    'CODE 0:$!b', 'the plan reads back as bound';

class MC is M { has $.e = 'e' }
my $mc = MC.new(a => 5, b => 6);
is "{$mc.a} {$mc.b} {$mc.e}", '50 6 e', 'a subclass runs the parent\'s replaced plan';

my @published;
role Publish {
    method publish_method_cache(Mu \type) { @published.push: type.^name; nextsame }
}
multi trait_mod:<is>(Attribute:D $attr, :$publish!) {
    $*PACKAGE.HOW does Publish unless $*PACKAGE.HOW ~~ Publish;
}
class Q { has $.x is publish }
is @published.join(','), 'Q', 'a HOW mixin\'s publish_method_cache runs as the class composes';
is Q.new(x => 7).x, 7, 'a class whose plan was not replaced constructs as before';
