use Test;

# An `@` pointy parameter of `given`/`with`/`if` binds like a signature `@`
# parameter: a Seq binds through PositionalBindFailover (its cached List),
# whether the topic is a bare Seq or a `$`-held one. A plain `my @l = $seq`
# would keep the `$`-held Seq as ONE element (Backtrace::Files
# t/03-add-source-lines.rakutest saw one "line" holding the whole file).

plan 12;

sub held { my $s = (1, 2, 3).Seq; $s }
my $s = (1, 2, 3).Seq;

with $s -> @l { is @l.elems, 3, 'with $seq -> @l' }
with held() -> @l { is @l.elems, 3, 'with call() -> @l, a $-held Seq' }
if held() -> @l { is @l.elems, 3, 'if call() -> @l' }
given (1, 2, 3).Seq -> @l {
    is @l.elems, 3, 'given Seq -> @l';
    is @l.WHAT.raku, 'List', 'the parameter holds the cached List';
}
given $s -> @l { is @l.elems, 3, 'given $seq -> @l' }
is $s.WHAT.raku, 'Seq', 'the topic variable keeps its Seq';
is $s.elems, 3, 'and the Seq is still readable';

my %cache;
%cache<k> := (4, 5).Seq;
with %cache<k> -> @l { is @l.elems, 2, 'with %h<k> -> @l, a Seq bound into a Hash' }

with held() -> @l is copy { @l.push(4); is @l.elems, 4, 'is copy gets the elements' }

my @a = 1, 2;
given @a -> @p { @p.push(3) }
is @a.elems, 3, 'given @a -> @p still aliases the Array';

with $[1, 2] -> @l { is @l.elems, 2, 'an itemized Array binds as itself' }
