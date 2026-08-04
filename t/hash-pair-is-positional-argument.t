use Test;
plan 6;

# In Raku, whether an argument is named is a property of the CALL SITE, not of
# the value: only a literal `k => v` / `:k(...)` written in the argument list is
# a named argument. A Pair that arrives through a variable — including one read
# out of a Hash — is an ordinary positional argument.
#
# mutsu encodes named-ness in the value (`Pair` = named, `ValuePair` =
# positional), so every hash-derived pair used to be misread as a named
# argument. That is what made `Cro::HTTP::Client` reject `headers => %h`: its
# `set-headers` matches `when Pair` and then calls `$request.append-header($_)`,
# whose typed multi candidates saw a named argument and no candidate matched.

class Sink {
    multi method take(Pair $p) { "Pair:" ~ $p.key ~ '=' ~ $p.value }
    multi method take(Str $s) { "Str:$s" }
    multi method take(Str $n, Str $v) { "Str,Str" }
}
my $sink = Sink.new;
my %h = a => 1;

is $sink.take(%h.pairs[0]), 'Pair:a=1', 'a pair from %h.pairs binds the Pair candidate';
is $sink.take(%h.List[0]), 'Pair:a=1', 'a pair from %h.List binds the Pair candidate';
is %h.map({ $sink.take($_) }).join, 'Pair:a=1', 'a pair from iterating %h binds the Pair candidate';
is $sink.take(%h.antipairs[0]), 'Pair:1=a', 'a pair from %h.antipairs binds the Pair candidate';
is $sink.take(%h.invert[0]), 'Pair:1=a', 'a pair from %h.invert binds the Pair candidate';

# `-> (:$k)` destructures its single argument; it is not a named parameter.
# (`-> :$k` is the named parameter.) Flattening the former into the latter only
# looked right while every hash-derived pair was itself a named argument — the
# argument here is a Hash, whose Capture exposes its keys as named parts.
is ({ a => 1 },).map(-> (:$a) { $a }).join, '1',
    'a lone named sub-signature param destructures the argument';
