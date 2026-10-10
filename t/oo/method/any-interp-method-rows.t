use Test;

# `Any`'s iteration methods -- `map`, `grep`, `first`, `reduce`, `produce`,
# `rotor`, `skip`, `squish`, `eager`, `iterator`, `match`, `classify`,
# `categorize` -- and `Hash`'s `classify-list` / `categorize-list` are
# `Handler::Interp` rows of the one method table (ADR-11276, the `Any`
# interpreter rows). The table sees them, the resolver can reach them as native
# candidates, and each handler is the one implementation the cascade's arm calls.

plan 36;

my @a = 1..6;

# --- the calls answer what Rakudo answers ---------------------------------------
is-deeply @a.map(* * 2).list, (2, 4, 6, 8, 10, 12), 'map';
is-deeply @a.grep(* %% 2).list, (2, 4, 6), 'grep';
is-deeply @a.grep(* %% 2, :k).list, (1, 3, 5), 'grep with a named adverb';
is-deeply @a.grep(* %% 2, :p).list, (1 => 2, 3 => 4, 5 => 6), 'grep :p';
is @a.first(* > 3), 4, 'first with a matcher';
is @a.first(* > 3, :k), 3, 'first with a matcher and :k';
is @a.first(* > 3, :end), 6, 'first with :end';
is @a.reduce(&[+]), 21, 'reduce';
is-deeply @a.produce(&[+]).list, (1, 3, 6, 10, 15, 21), 'produce';
is-deeply @a.rotor(2).list, ((1, 2), (3, 4), (5, 6)), 'rotor';
is-deeply (1..5).rotor(2, :partial).list, ((1, 2), (3, 4), (5,)), 'rotor :partial';
is-deeply @a.skip(2).list, (3, 4, 5, 6), 'skip';
is-deeply @a.skip.list, (2, 3, 4, 5, 6), 'skip without an argument';
is-deeply <a a b b c>.squish.list, <a b c>, 'squish';
is-deeply (1, 2, 3).eager, (1, 2, 3), 'eager';
is @a.iterator.pull-one, 1, 'iterator';
is "abc".match(/b/).Str, 'b', 'match';
is-deeply (1..6).classify({ $_ %% 2 ?? 'even' !! 'odd' }).sort.list,
    (even => [2, 4, 6], odd => [1, 3, 5]).sort.list, 'classify';
is-deeply (1..4).categorize({ $_ %% 2 ?? 'even' !! 'odd' }).sort.list,
    (even => [2, 4], odd => [1, 3]).sort.list, 'categorize';
my %h;
%h.classify-list({ $_ %% 2 ?? 'even' !! 'odd' }, 1..6);
is-deeply %h.sort.list, (even => [2, 4, 6], odd => [1, 3, 5]).sort.list, 'classify-list';
my %c;
%c.categorize-list({ $_ %% 2 ?? 'e' !! 'o' }, 1..4);
is-deeply %c.sort.list, (e => [2, 4], o => [1, 3]).sort.list, 'categorize-list';

# --- receivers other than an array ------------------------------------------------
is-deeply 5.map(* + 1).list, (6,), 'map over a scalar';
is-deeply (1..3).map(* * 3).list, (3, 6, 9), 'map over a Range';
is-deeply (a => 1, b => 2).map({ .key }).sort.list, <a b>, 'map over Pairs';
is-deeply %(a => 1).grep(*.value == 1).list, (a => 1,), 'grep over a Hash';
is-deeply <a b c>.Seq.skip(1).list, <b c>, 'skip over a Seq';

# --- laziness and consumption keep their behaviour ----------------------------------
is (1..Inf).map(* * 2).head(3).list, (2, 4, 6), 'a lazy source stays lazy through map';
is (1..Inf).grep(* %% 2).head(3).list, (2, 4, 6), 'a lazy source stays lazy through grep';
my $seq = (1, 2, 3).Seq;
is $seq.map(* + 1).list, (2, 3, 4), 'a Seq maps once';
throws-like { $seq.map(* + 1).list }, X::Seq::Consumed, 'a Seq cannot be consumed twice';

# --- a user class that supplies `iterator` is iterated through it ----------------------
class Counter { method iterator { (1, 2, 3).iterator } }
is-deeply Counter.new.map(* * 2).list, (2, 4, 6), 'map on a user iterator';
is-deeply Counter.new.grep(* > 1).list, (2, 3), 'grep on a user iterator';

# --- an error is the interpreter's -----------------------------------------------------------
throws-like { (1, 2, 3).map(5) }, X::Cannot::Map, 'map refuses a non-callable';

# --- the table lists them -------------------------------------------------------------
ok Any.^can('map'), '.^can sees map';
ok Any.^can('grep') && Any.^can('first') && Any.^can('reduce'), '.^can sees grep, first, reduce';
ok Hash.^can('classify-list'), '.^can sees classify-list';
