use v6.e.PREVIEW;
use Test;

# `Capture` and `fmt` of the receivers the pure rows decline are the
# interpreter twins of `Mu.Capture` and `Any.fmt` (ADR-11276 §9.187): a user
# object, a live source, user `.Str` elements, a `Format` object.

plan 8;

class P { has $.a = 1; has $!b = 2; method a { 42 } }
is P.new.Capture.raku, '\(:a(42))', 'Capture of an object reads its public accessors';

my $c = Channel.new; $c.send(1); $c.send(2); $c.close;
is $c.Capture.raku, '\(1, 2)', 'Capture of a Channel drains it';

class K { method Str { 'kk' } }
is (K.new => 1, ).Capture.raku, '\(:kk(1))', 'a non-Str Pair key is stringified through its Str';
is Supply.from-list(1, 2).Capture.raku, '\(1, 2)', 'Capture of a Supply';

class S { method Str { 'obj' } }
is (S.new, S.new).fmt('<%s>'), '<obj> <obj>', 'fmt over objects with a Str';
is {a => S.new}.fmt('%s=%s'), 'a=obj', 'fmt of a Hash with object values';
is (1, 2).fmt(Format.new('[%d]')), '[1] [2]', 'fmt with a Format object';
is 3.fmt(Format.new('%05d')), '00003', 'a scalar fmt with a Format object';
