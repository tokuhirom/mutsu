use v6;
use Test;

plan 7;

# A user `new` deferring to its native ancestor's constructor
# (`nextwith`/`callwith`/`callsame`) reaches `Range.new`/`Pair.new`, not `Mu.new`.
class Reals is Range {
    method new(:$min = -Inf, :$max = Inf, :$excludes-min = False, :$excludes-max = False) {
        nextwith $min, $max, :$excludes-min, :$excludes-max
    }
}
is Reals.new(:min(1), :max(5)).raku, '1..5', 'nextwith builds the Range';
is Reals.new.WHAT.raku, 'Reals', 'the result is an instance of the subclass';
ok Reals.new(:min(1), :max(5)) ~~ Range, 'and a Range';

class R is Range { method new(|c) { callsame } }
is R.new(1, 3).raku, '1..3', 'callsame defers to Range.new';

class P is Pair { method new($k, $v) { callwith($k, $v) } }
is P.new(1, 2).raku, '1 => 2', 'callwith defers to Pair.new';

class S { has $.v; method new($v) { nextwith :$v } }
is S.new(9).v, 9, 'a plain class still defers to Mu.new';
class B is S { method new(|c) { callsame } }
is B.new(4).v, 4, 'a subclass chain still reaches Mu.new';
