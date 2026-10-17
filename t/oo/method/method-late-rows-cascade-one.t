use Test;

# `Mu.say`/`put` with arguments, `Junction.THREAD` and `Cool.encode` are late
# rows of the receiver's MRO (ADR-11276 §9.142). Answers checked against Rakudo.

plan 7;

class Out { has @.got; method print(*@a) { @!got.push(@a.join) } }
my $o = Out.new;
$o.say(1, "a");
$o.put(2, 3);
is-deeply $o.got.List, ("1a\n", "23\n"), 'say and put hand their text to a user print';

my @seen;
any(1, 2, 3).THREAD({ @seen.push($_) });
is-deeply @seen.sort.List, (1, 2, 3), 'Junction.THREAD calls the block per eigenstate';

is "abc".encode.^name, 'utf8', 'Str.encode';
is "abc".encode("latin-1").elems, 3, 'encode with an encoding';
throws-like { Str.encode }, Exception, message => 'Cannot unbox a type object (Str) to a str.',
    'encode on a type object';
is 42.encode.elems, 2, 'Cool.encode of a number';
is-deeply "é".encode("utf-16").elems, 1, 'encode to utf-16';
