use Test;

# A method-candidate wrap chain is looked up only for a class that ever had
# one (#12875). Before that, wrapping one class's `pick` made every `.pick`
# call on any value probe the chains of every class of its built-in MRO.
# These pin that the lookup still finds every chain it should, and only
# those.

plan 9;

class Frob {
    method pick() { 'frob' }
}
Frob.^lookup('pick').wrap(my method (|c) { 'wrapped ' ~ callsame });

is Frob.new.pick, 'wrapped frob', 'the wrapped class runs its wrapper';

my @words = <a b c>;
ok @words.pick ∈ @words, 'an Array .pick of the same name is not wrapped';
ok (1..3).pick ∈ (1..3), 'a Range .pick is not wrapped';

class Other {
    method pick() { 'other' }
}
is Other.new.pick, 'other', 'another class with the same method name is not wrapped';

class Sub is Frob { }
is Sub.new.pick, 'wrapped frob', 'a subclass inherits the wrapped candidate';

# A class that is wrapped after the first lookups missed.
Other.^lookup('pick').wrap(my method (|c) { 'late ' ~ callsame });
is Other.new.pick, 'late other', 'a class wrapped later is found';
is Frob.new.pick, 'wrapped frob', 'the first wrap still holds';

# A built-in method wrapped on a built-in class, found through the MRO.
my $h = Uni.^lookup('list').wrap(-> \self { 'uni-wrapped' });
is "e\x[301]".NFD.list, 'uni-wrapped', 'a wrap of Uni.list reaches NFD.list';
$h.restore;
is "e\x[301]".NFD.list.elems, 2, 'and is gone after restore';
