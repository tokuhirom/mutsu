use Test;

plan 7;

# A parameter may carry a trait name the signature machinery does not know, as
# long as some `trait_mod:<is>` accepts a Parameter. Cro::HTTP::Router declares
# `multi trait_mod:<is>(Parameter:D $param, :$query!)` and friends, and its route
# handlers are written `get -> 'search', :$min-price is query = 0 { ... }`.
my @applied;
multi trait_mod:<is>(Parameter:D $param, :$query! --> Nil) {
    @applied.push($param.name);
}

sub search(:$min-price is query = 0, :$max-price is query = Inf) {
    "$min-price..$max-price"
}

is search(), '0..Inf', 'a sub with custom parameter traits is callable';
is search(min-price => 3), '3..Inf', 'its named arguments still bind';
ok @applied.grep('$min-price'), 'trait_mod:<is> ran for the first parameter';
ok @applied.grep('$max-price'), 'trait_mod:<is> ran for the second parameter';

# The same on a pointy block, which is how Cro route handlers are written.
my $handler = -> 'search', :$min-price is query = 0 { "got $min-price" };
is $handler('search', min-price => 7), 'got 7', 'a pointy block takes custom parameter traits';

# A trait no trait_mod can accept is still an error, at the declaration.
throws-like { EVAL 'sub oh-noes($gack is nonesuch) { }' }, Exception,
    'an unknown parameter trait still dies',
    message => /nonesuch/;

# A built-in parameter trait keeps working next to a custom one.
sub mixed($x is copy, :$y is query = 1) { $x = $x + $y; $x }
is mixed(1), 2, 'is copy still works alongside a custom trait';
