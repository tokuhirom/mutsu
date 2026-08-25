use v6;
use Test;

# Pins the compiled-method fast path's named/attributive parameter binding
# (bench-ctor S6, todo/tickets/bench-ctor-construction-parity.md): simple
# named scalar params and attributive named params (`:$!x`, `:@!a`, `:%!h`)
# now bind on the fast path. Every behavior here was verified against rakudo
# (v2026.06) before the fast path was extended, and must stay identical
# whichever dispatch path a call takes.

plan 33;

# --- attributive scalar named param on TWEAK ---
class TScalar {
    has $.x = 10;
    submethod TWEAK(:$!x) { }
}
# rakudo: an UNSUPPLIED attributive named param still binds — it overwrites
# the attribute default with the type object.
ok TScalar.new.x === Any, 'unsupplied :$!x overwrites the default with Any (rakudo semantics)';
is TScalar.new(x => 5).x, 5, 'supplied :$!x binds the named arg into the attribute';

# --- attributive array/hash named params on TWEAK ---
class TContainers {
    has @.r = (1, 2);
    has %.m = (a => 1);
    submethod TWEAK(:@!r, :%!m) { }
}
is-deeply TContainers.new.r, [], 'unsupplied :@!r binds a fresh empty Array';
is-deeply TContainers.new.m, {}, 'unsupplied :%!m binds a fresh empty Hash';
my $tc = TContainers.new(r => [3, 4], m => { b => 2 });
is-deeply $tc.r, [3, 4], 'supplied :@!r binds through the attribute cell';
is-deeply $tc.m, { b => 2 }, 'supplied :%!m binds through the attribute cell';
# List re-homing: a List handed to :@!r becomes the array's elements.
my $tl = TContainers.new(r => (7, 8));
is $tl.r.elems, 2, ':@!r re-homes a List into the array';
is $tl.r[1], 8, ':@!r element access after re-homing';

# --- plain named scalar param on TWEAK, and %_ ---
class TPlain {
    has $.y;
    has %.rest;
    submethod TWEAK(:$z) { $!y = $z // 'none'; %!rest = %_; }
}
is TPlain.new.y, 'none', 'unsupplied :$z binds Any (// picks the default)';
is TPlain.new(z => 7).y, 7, 'supplied :$z binds the named arg';
is-deeply TPlain.new(z => 1, extra => 2).rest, { extra => 2 },
    '%_ holds only named args not consumed by an explicit named param';

# --- %_ excludes keys consumed by ATTRIBUTIVE params (rakudo behavior) ---
class TSlurpySeen {
    has $.x;
    has %.seen;
    submethod TWEAK(:$!x) { %!seen = %_; }
}
is-deeply TSlurpySeen.new(x => 5, q => 9).seen, { q => 9 },
    '%_ excludes a key consumed by an attributive named param';

# --- multi-level MRO TWEAK chain (the bench-ctor shape) ---
class Base1 {
    has $.spec;
    submethod TWEAK(:$!spec) { }
}
class Child1 is Base1 {
    has %!meta;
    has @.resources;
    has $.name;
    method new(*%_) { self.bless(|%_, :meta(%_)) }
    method meta { %!meta }
    submethod TWEAK(:%!meta, :@!resources --> Nil) {
        @!resources = @!resources.map(*.flat);
    }
}
my $c = Child1.new(name => 'N', spec => 'S', resources => [(1, 2), (3,)]);
is $c.name, 'N', 'MRO chain: ordinary attribute set by bless';
is $c.spec, 'S', 'MRO chain: parent TWEAK attributive param bound';
is $c.meta<name>, 'N', 'MRO chain: child TWEAK :%!meta bound from :meta(%_)';
is $c.resources.elems, 2, 'MRO chain: child TWEAK body ran over @!resources';

# --- defaults still evaluate (falls back to the full path when unsupplied) ---
class TDefault {
    has $.v;
    submethod TWEAK(:$w = 40 + 2) { $!v = $w; }
}
is TDefault.new.v, 42, 'unsupplied defaulted named param evaluates its default expr';
is TDefault.new(w => 1).v, 1, 'supplied defaulted named param binds the arg';

# --- required named param still dies when missing ---
class TRequired {
    has $.v;
    submethod TWEAK(:$r!) { $!v = $r; }
}
is TRequired.new(r => 3).v, 3, 'supplied required named param binds';
dies-ok { TRequired.new }, 'missing required named param still dies';

# --- type constraints keep the full path (still enforced) ---
class TTyped {
    has $.v;
    submethod TWEAK(Int :$t) { $!v = $t; }
}
is TTyped.new(t => 3).v, 3, 'typed named param binds a conforming arg';
dies-ok { TTyped.new(t => 'nope') }, 'typed named param still rejects a mismatch';

# --- named params are readonly on every path ---
class TReadonly {
    method m(:$x) { $x = 5 }
}
dies-ok { TReadonly.new.m(x => 1) }, 'assigning to a named param still dies (readonly)';

# --- ordinary methods (not just TWEAK) with named params ---
class TMethod {
    has $.acc = '';
    method greet(:$name, :$punct) { "hi { $name // '?' }{ $punct // '' }" }
    method both($pos, :$named) { "$pos/{ $named // 'n' }" }
}
is TMethod.new.greet(name => 'bob', punct => '!'), 'hi bob!', 'method named params bind';
is TMethod.new.greet(punct => '?'), 'hi ??', 'method unsupplied named param is Any';
is TMethod.new.both('p', named => 'x'), 'p/x', 'mixed positional + named binding';
is TMethod.new.both('p'), 'p/n', 'mixed shape with named unsupplied';
# rightmost duplicate named arg wins (slow-binder rule)
is TMethod.new.greet(name => 'a', name => 'b'), 'hi b', 'rightmost duplicate named arg wins';

# --- named container arg into a scalar named param still aliases the caller ---
class TShare {
    method push-it(:$n) { $n.push: 99 }
}
my @shared = 1, 2;
TShare.new.push-it(n => @shared);
is @shared.elems, 3, 'named @-variable arg into :$n still mutates the caller array';

# --- BUILD attributive bind counts as "BUILD set it" (initializer suppressed) ---
class TBuildBind {
    has $.a = 5;
    submethod BUILD(:$!a) { }
}
ok TBuildBind.new.a === Any, 'unpassed :$!a in BUILD suppresses the has-initializer';
is TBuildBind.new(a => 9).a, 9, 'supplied :$!a in BUILD binds the named arg';

# --- rename/alias params keep working (full path) ---
class TAlias {
    has $.v;
    submethod TWEAK(:long(:$short)) { $!v = $short // 'none'; }
}
is TAlias.new(long => 'L').v, 'L', 'alias named param binds via outer key';
is TAlias.new(short => 'S').v, 'S', 'alias named param binds via inner key';

done-testing;
