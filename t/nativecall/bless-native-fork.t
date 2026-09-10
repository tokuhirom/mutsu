use v6;
use Test;

# Pin for the native `bless` VM fork (skips the interpreter's generic
# method-dispatch scan and reuses the cached per-class NativeCtorPlan).
# These cases exercise every gate of the fork: plain bless, bless with
# role-composed BUILD/TWEAK submethods, bless with attribute defaults and
# typed containers, a user-defined `bless` override (must NOT take the
# fork), and bless from a custom `new` on a subclass.

plan 11;

my @events;

role R {
    submethod BUILD { @events.push('role-BUILD') }
    submethod TWEAK { @events.push('role-TWEAK') }
}

class WithRole does R {
    has $.y;
    method new(*%_) { self.bless(|%_) }
}

@events = ();
my $wr = WithRole.new(:7y);
is @events.join(','), 'role-BUILD,role-TWEAK',
    'role-composed BUILD and TWEAK submethods run during bless';

class Base {
    has $.p = 10;
}

class WithDefaults is Base {
    has Int @.nums;
    has $.z = 3;
    has %.map;
    method new(*%_) { self.bless(|%_) }
}

my $wd = WithDefaults.new(:nums([1, 2, 3]));
is ~$wd.nums, '1 2 3', 'typed @ attribute set through bless';
is $wd.z, 3, 'literal scalar default applied by bless';
is $wd.p, 10, 'inherited attribute default applied by bless';
is $wd.map.elems, 0, '% attribute with no default is an empty hash';
is WithDefaults.new(:5z).z, 5, 'bless folds named args into attributes';

class CustomBless {
    has $.w;
    method bless(*%_) {
        @events.push('custom-bless');
        callsame;
    }
    method new(*%_) { self.bless(|%_) }
}

@events = ();
my $cb = CustomBless.new(:9w);
is @events.join(','), 'custom-bless',
    'user-defined bless override still dispatches (no native fork)';

class TweakOnly {
    has $.spec;
    has @.resources;
    submethod TWEAK(:$!spec, :@!resources --> Nil) {
        @!resources = @!resources.map(* x 2);
    }
    method new(*%_) { self.bless(|%_) }
}

my $to = TweakOnly.new(:spec('s'), :resources(['a', 'b']));
is $to.spec, 's', 'attributive named TWEAK param binds through bless';
is-deeply $to.resources, ['aa', 'bb'], 'TWEAK mutates @!attr during bless';

# bless called directly on the type object (no custom new in between).
class DirectBless { has $.v = 5 }
my $db = DirectBless.bless(:11v);
is $db.v, 11, 'direct Type.bless(...) works';
is DirectBless.bless().v, 5, 'direct Type.bless() applies defaults';

done-testing;
