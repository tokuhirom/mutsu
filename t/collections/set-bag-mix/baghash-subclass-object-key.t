use Test;
plan 9;

my $o = Any.new;

# A QuantHash subclass declared with an object hash keeps the key objects.
class ABH is BagHash { }
my %plain is ABH = :{ $o => 42 };
is %plain{$o}, 42, 'object key survives `my %h is Subclass = :{ ... }`';
is %plain.keys[0].WHICH, $o.WHICH, 'the key is the object itself';

# STORE flattens a Hash into its pairs, keeping object keys.
my $b = BagHash.new;
$b.STORE(:{ $o => 3 });
is $b{$o}, 3, 'STORE of an object hash';

# An overridden AT-KEY (the documented Proxy idiom) with an object key.
my role Acc {
    multi method AT-KEY(::?CLASS:D: $key is raw) is raw {
        my &n := nextcallee;
        Proxy.new(
            FETCH => { n(self, $key) },
            STORE => -> $, Numeric() $v { n(self, $key) = $v },
        )
    }
}
class Counting is BagHash does Acc { }
my %c is Counting = :{ $o => 42 };
is %c{$o}, 42, 'overridden AT-KEY reads an object key';
%c{$o} = 7;
is %c{$o}, 7, '... and writes through the Proxy';
%c<s> = 3;
is %c<s>, 3, 'a Str key still works';
is %c{Any.new}, 0, 'an absent object key is 0';

my %s is Counting = a => 1;
is %s<a>, 1, 'plain Str initialization';
is %s.elems, 1, 'one element';
