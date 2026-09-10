use Test;

plan 24;

sub roles($t, *%a) { $t.^roles(|%a).map(*.^name).join(',') }

# `.^roles(:!transitive)` stops at the DIRECTLY composed roles. The built-in
# types kept a flat, pre-flattened list with no direct/inherited distinction,
# so the adverb did nothing for them.
is roles(Int, :!transitive), 'Real', 'Int composes Real directly, Numeric through it';
is roles(Int), 'Real,Numeric', '... and the full closure is unchanged';
is roles(Num, :!transitive), 'Real', 'Num likewise';
is roles(Rat, :!transitive), 'Rational[Int,Int]', 'Rat stops at the parametric role';
is roles(Rat), 'Rational[Int,Int],Real,Numeric', '... whose closure is unchanged';
is roles(FatRat, :!transitive), 'Rational[Int,Int]', 'FatRat likewise';
is roles(Complex, :!transitive), 'Numeric', 'a single direct role is unaffected';
is roles(Str, :!transitive), 'Stringy', 'and so is Str';

# The Positional/Associative built-ins answered an EMPTY list for `.^roles`
# entirely -- they had no entry at all.
is roles(Array), 'Positional,Iterable', 'Array composes Positional and Iterable';
is roles(Array, :!transitive), 'Positional,Iterable', '... both directly';
is roles(Hash), 'Associative,Iterable', 'Hash';
is roles(Map), 'Associative,Iterable', 'Map';
is roles(List), 'Positional,Iterable', 'List';
is roles(Slip), 'Positional,Iterable', 'Slip';
is roles(Range), 'Positional,Iterable', 'Range';
is roles(Pair), 'Associative', 'Pair';
is roles(Seq), 'Sequence,PositionalBindFailover,Iterable', 'Seq closure';
is roles(Seq, :!transitive), 'Sequence,Iterable', '... PositionalBindFailover comes through Sequence';
is roles(Set, :!transitive), 'Setty', 'Set stops at Setty';
is roles(Bag, :!transitive), 'Baggy', 'Bag stops at Baggy';
is roles(Mix, :!transitive), 'Mixy', 'Mix stops at Mixy';
is roles(Buf, :!transitive), 'Blob[T]', 'Buf stops at Blob, not Blob\'s own roles';

# The user-declared side was already right and stays so.
{
    role X { }
    role Y does X { }
    class K does Y { }
    is roles(K, :!transitive), 'Y', 'a user class reports its direct role';

    # A class inheriting from a built-in reports the ancestor's direct roles.
    class L is Int { }
    is roles(L, :!transitive), 'Real', 'a subclass of a built-in inherits its direct roles';
}
