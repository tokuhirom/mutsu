use Test;

# EERPG's object-valued commodity keys exposed this object-hash identity gap.
# An object hash must key non-Str objects by .WHICH before stringification.
# Distinct objects with the same fallback .Str must remain separate entries.

plan 4;

class SameString {
    method Str { 'same' }
}

my $first = SameString.new;
my $second = SameString.new;
my %hash := :{ $first => 1, $second => 2 };

is %hash.elems, 2, 'object hash keeps objects with the same .Str distinct';
is %hash{$first}, 1, 'the first object key retrieves its value';
is %hash{$second}, 2, 'the second object key retrieves its value';
is %hash.keys.grep(* === $first).elems, 1, 'the original object key is preserved';
