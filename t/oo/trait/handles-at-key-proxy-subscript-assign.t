use Test;

# `$obj<k> = v` where `AT-KEY` is a `handles` forwarder to a method that returns
# an `is raw` Proxy: the store goes through the Proxy. Identical under rakudo.
plan 5;

class Store {
	has %.idx;
	has @.vals;
	method AT-KEY(\key) is raw {
		Proxy.new(
			FETCH => { %!idx{key}:exists ?? @!vals[%!idx{key}] !! Nil },
			STORE => -> $, \v {
				%!idx{key} //= +@!vals;
				@!vals[%!idx{key}] = v;
			},
		)
	}
	method EXISTS-KEY(\key) { %!idx{key}:exists }
}
role Rel does Associative { }

class Plain { has $.store handles <AT-KEY EXISTS-KEY>; }
class Assoc does Associative { has $.store handles <AT-KEY EXISTS-KEY>; }
class WithRole does Rel { has $.store handles <AT-KEY EXISTS-KEY>; }

for Plain, Assoc, WithRole -> \C {
	my $s = Store.new;
	my $t = C.new(store => $s);
	$t<x> = 5;
	is $s.idx<x>, 0, C.^name ~ ': the Proxy STORE ran';
}
my $s = Store.new;
my $t = Assoc.new(store => $s);
$t<a> = 1;
$t<b> = 2;
is $t<b>, 2, 'and a later read goes through FETCH';
is $t<a>:exists, True, 'EXISTS-KEY forwards too';
