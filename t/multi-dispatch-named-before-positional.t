use Test;

plan 8;

# Multi-dispatch matches positional parameters against the *positional*
# argument slots. A named argument written before a positional — which is what
# forwarding a capture after adding a named (`f(:x(1), |c)`) produces — used to
# shift every positional param onto the wrong slot, so no candidate matched.

proto sub enc(|) { * }
multi sub enc(:$aes256! where .so, |c) { enc(:cipher('c256'), |c) }
multi sub enc(Blob $plain, :$key, :$iv, :$cipher! where .so) {
    "enc({$plain.elems}):$cipher:{$key.elems}:{$iv.elems}"
}

my $plain = Blob.new(1, 2, 3, 4);
my $key = Blob.new(9, 9);
my $iv = Blob.new(7, 7, 7);
is enc($plain, :aes256, :$iv, :$key), 'enc(4):c256:2:3',
    'a capture forwarded after adding a named still binds the positional';
is-deeply $plain, Blob.new(1, 2, 3, 4), 'the forwarded argument is untouched';

# The same, written directly rather than through a forwarder.
multi sub two(Int $a, Str $b, :$flag) { "two:$a:$b:$flag" }
sub fwd(|c) { two(:flag, |c) }
is fwd(5, 'z'), 'two:5:z:True', 'a named written first does not shift two positionals';

# Type-based selection must still see the right argument in each slot.
multi sub pick(Int $n, :$tag) { "int:$n:$tag" }
multi sub pick(Str $s, :$tag) { "str:$s:$tag" }
is pick(:tag<t>, 3), 'int:3:t', 'the Int candidate wins with a leading named';
is pick(:tag<t>, 'a'), 'str:a:t', 'the Str candidate wins with a leading named';

# A slurpy takes the positionals from its own slot on, not from the raw one.
multi sub rest(Int $first, *@more, :$sep = '-') { ($first, |@more).join($sep) }
is rest(:sep<+>, 1, 2, 3), '1+2+3', 'a slurpy after a leading named collects the right tail';

# An arity that only matches once the named args are discounted.
multi sub arity(Int $a, :$x) { "one:$a" }
multi sub arity(Int $a, Int $b, :$x) { "two:$a,$b" }
is arity(:x, 1), 'one:1', 'a named argument does not count toward positional arity';
is arity(:x, 1, 2), 'two:1,2', 'the two-positional candidate still wins on two positionals';
