use Test;

# A class's `multi method ACCEPTS` candidates are added to the ones it inherits
# from `Any`/`Mu`, so a topic none of them binds is matched by the core
# candidates (`self === topic` for a defined matcher) instead of dying with
# "Cannot resolve caller ACCEPTS". Tinky's `Transition` declares only
# `ACCEPTS(State:D)` and `ACCEPTS(Object:D)`, and its suite smartmatches
# transitions against each other.

plan 7;

class S { }
class T {
    multi method ACCEPTS(S:D $s --> Bool) { True }
}
my $t = T.new;
my $u = T.new;

ok  $t ~~ $t, 'an unmatched topic falls back to identity: itself';
nok $u ~~ $t, 'an unmatched topic falls back to identity: another instance';
ok  S.new ~~ $t, 'a topic a user candidate binds still runs it';
is (T.new, $t, 42).grep($t).elems, 1, 'grep with the matcher uses the same fallback';
nok (1, 2) ~~ T, 'a type-object matcher is unaffected';

class Only {
    method ACCEPTS(S:D $s) { True }
}
throws-like { 42 ~~ Only.new }, Exception,
    'a non-multi ACCEPTS still owns the name, so a binding failure is reported';

class Sub is T { }
nok T.new ~~ Sub.new, 'an inherited multi ACCEPTS falls back the same way';
