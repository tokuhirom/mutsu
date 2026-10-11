use Test;

# A class that composes a role after a run-time statement composes it at BEGIN
# time, like Rakudo, so the role body runs once, ahead of the mainline, over
# the unit's lexicals in their static state (#10667).

plan 6;

my @log;
my $seen = 0;
@log.push("start");

role R {
    @log.push("body");
    method m { 1 }
}

@log.push("mid");

class A does R { }
class B is A does R { has $.x }

@log.push("end");

is-deeply @log, ["body", "body", "start", "mid", "end"],
    'each composition ran the body once, before the mainline';
is A.new.m, 1, 'the composed method works';
is B.new.m, 1, 'a subclass composing the role again works';

# A body that rebinds an outer lexical aliases the unit's own container.
my $z = 5;
role Alias {
    my $w := $z;
    $w = 7;
    method get { 1 }
}
class C does Alias { }
is C.new.get, 1, 'a role body binding an outer lexical composes';

# A trait on a declaration keeps its initializer for the trait's STORE.
role Init { method STORE(\v, :$INITIALIZE) { $seen++ if $INITIALIZE; self } }
class D does Init { }
my %h is Init = 1, 2;
is $seen, 1, 'the initializer reaches the trait role with :INITIALIZE';
is C.new.get, 1, 'still composed';
