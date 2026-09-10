use Test;

plan 23;

# `R.new` puns the role into a class and constructs. That construction is the
# ordinary class construction, so the role's BUILD and TWEAK submethods run --
# exactly once each, in the same order and with the same relationship to
# attribute initializers as `class C does R { }`. Every expectation below was
# checked against Rakudo v2026.06.

# --- BUILD ---------------------------------------------------------------

role BuildsHash { has %.h; submethod BUILD(--> Nil) { %!h{"a"} = 1 } }
is-deeply BuildsHash.new.h, {a => 1}, 'BUILD fills a punned hash attribute';

role BuildsScalar { has $.s; submethod BUILD(--> Nil) { $!s = 7 } }
is BuildsScalar.new.s, 7, 'BUILD assigns a punned scalar attribute';

role BuildsArray { has @.a; submethod BUILD(--> Nil) { @!a.push(1); @!a.push(2) } }
is-deeply BuildsArray.new.a, [1, 2], 'BUILD pushes onto a punned array attribute';

# The pun must not run the submethod twice -- it is reachable both as the
# punned class's own method and as a composed-role submethod.
my $build-runs = 0;
role CountsBuild { submethod BUILD(--> Nil) { $build-runs++ } }
CountsBuild.new;
is $build-runs, 1, 'BUILD runs exactly once for a pun';

my $tweak-runs = 0;
role CountsTweak { submethod TWEAK(--> Nil) { $tweak-runs++ } }
CountsTweak.new;
is $tweak-runs, 1, 'TWEAK runs exactly once for a pun';

role NamedBuild { has $.a; has $.b; submethod BUILD(:$!a) { $!b = "b:" ~ $!a } }
is NamedBuild.new(a => 3).b, 'b:3', 'BUILD binds a named constructor argument';

# --- TWEAK ---------------------------------------------------------------

role TweaksHash { has %.h; submethod TWEAK(--> Nil) { %!h{"a"} = 1 } }
is-deeply TweaksHash.new.h, {a => 1}, 'TWEAK fills a punned hash attribute';

role TweakDefaults { has $.x; submethod TWEAK { $!x //= "tweaked" } }
is TweakDefaults.new.x, 'tweaked', 'TWEAK sees an unsupplied attribute';
is TweakDefaults.new(x => 'given').x, 'given', 'TWEAK sees a supplied attribute';

# --- ordering against attribute initializers -----------------------------

# BUILD runs BEFORE the initializers, and an attribute BUILD wrote does not
# then get its initializer; TWEAK runs after, so it sees the initialized value.
role BuildBeatsDefault { has $.x = 5; submethod BUILD(--> Nil) { $!x = 9 } }
is BuildBeatsDefault.new.x, 9, 'BUILD wins over an attribute initializer';

role TweakSeesDefault { has $.x = 5; submethod TWEAK(--> Nil) { $!x = $!x + 1 } }
is TweakSeesDefault.new.x, 6, 'TWEAK sees the initialized value';

role TweakSeesTypedDefault { has Int $.i = 2; submethod TWEAK(--> Nil) { $!i = $!i * 10 } }
is TweakSeesTypedDefault.new.i, 20, 'TWEAK sees a typed initialized value';

# --- composition is unaffected -------------------------------------------

role Shared { has @.a; submethod BUILD(--> Nil) { @!a.push(1) } }
class Consumer does Shared { }
is-deeply Consumer.new.a, [1], 'a composed role still runs BUILD once';
is-deeply Shared.new.a, [1], 'and the same role punned runs it once too';

# --- parameterized roles -------------------------------------------------

role FromTypeParam[::T] { has $.x; submethod BUILD(--> Nil) { $!x = T.^name } }
is FromTypeParam[Int].new.x, 'Int', 'BUILD sees a type parameter';

role FromValueParam[$v] { has $.x; submethod BUILD(--> Nil) { $!x = $v } }
is FromValueParam[7].new.x, 7, 'BUILD sees a value parameter';

# A Hash argument has no faithful spelling in a type name, so this
# parameterisation takes a different pun route than the two above.
role FromHashParam[%d] {
    has %.h;
    submethod BUILD(--> Nil) { for %d.kv -> $k, $v { %!h{$k} = $v } }
}
is-deeply FromHashParam[%(a => 1)].new.h, {a => 1},
    'BUILD sees a hash parameter';

role FromArrayParam[@l] { has @.a; submethod BUILD(--> Nil) { @!a = @l } }
is-deeply FromArrayParam[[1, 2]].new.a, [1, 2],
    'BUILD sees an array parameter, unflattened';

role TweaksFromParam[$v] { has $.x; submethod TWEAK(--> Nil) { $!x = $v * 2 } }
is TweaksFromParam[4].new.x, 8, 'TWEAK sees a value parameter';

# An itemized array is ONE type argument, not one per element: spreading it
# left `role R[@l]` with two arguments and matching no candidate at all.
role ArrayParamElems[@l] { method n { @l.elems } }
is ArrayParamElems[[1, 2]].new.n, 2, 'an array type argument is a single argument';

role HashParamKeys[%d] { method n { %d.keys.sort.join(",") } }
is HashParamKeys[%(a => 1, b => 2)].new.n, 'a,b', 'a hash type argument survives punning';

# --- the pun is per-construction ------------------------------------------

# Constructing withdraws the pun again, and a construction *plan* cached under
# that name has to go with it: a stale one made every construction after the
# first take the plain-class fast path, losing the role entirely.
role Repeatable { has @.a; submethod BUILD(--> Nil) { @!a.push(1) }; method m { 'm' } }
my $first = Repeatable.new;
my $second = Repeatable.new;
is $second.m, 'm', 'a second pun construction still has the role methods';
is-deeply $second.a, [1], 'and still ran BUILD';
