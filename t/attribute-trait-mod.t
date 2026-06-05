use Test;

plan 7;

# User-defined trait_mod:<is> dispatched on Attribute objects.

my @noted-names;
multi trait_mod:<is>(Attribute $a, :$noted!) {
    push @noted-names, $a.name;
}

class C1 {
    has $!a is noted;
    has @!b is noted;
    has %!c is noted;
}

# Force composition.
ok C1.new ~~ C1, 'class with noted attributes instantiated';
@noted-names .= sort;
is +@noted-names, 3, 'trait_mod applied to each attribute once';
is @noted-names, ['$!a', '%!c', '@!b'], 'attribute names passed to trait_mod';

# Re-instantiating does not re-apply the trait.
C1.new;
is +@noted-names, 3, 'trait applied at composition, not at .new';

# Trait that takes a positional type + argument.
my %seen-args;
multi trait_mod:<is>(Attribute $a, :$tagged!) {
    %seen-args{$a.name} = $tagged;
}

class C2 {
    has $.x is tagged('hello');
    has $.y is tagged(42);
}
ok C2.new ~~ C2, 'class with parameterized trait instantiated';
is %seen-args{'$!x'}, 'hello', 'string argument passed to trait_mod';
is %seen-args{'$!y'}, 42, 'integer argument passed to trait_mod';

# vim: expandtab shiftwidth=4
