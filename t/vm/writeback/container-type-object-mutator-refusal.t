use Test;

# A container type object refuses the receiver-mutating methods Rakudo
# declares for a defined invocant only (#13881): an `only` method with an
# invalid-concreteness error, a multi with no candidate. The growing methods
# still vivify a type object a binding holds. Expected values are Rakudo's.

plan 16;

throws-like { Array.pop }, X::Parameter::InvalidConcreteness, 'Array.pop';
throws-like { Array.shift }, X::Parameter::InvalidConcreteness, 'Array.shift';
throws-like { Array.splice }, X::Multi::NoMatch, 'Array.splice';
throws-like { Array.grab }, X::Multi::NoMatch, 'Array.grab';
throws-like { Array.STORE((1,)) }, X::Multi::NoMatch, 'Array.STORE';
throws-like { Hash.STORE((a => 1)) }, X::Multi::NoMatch, 'Hash.STORE';
throws-like { SetHash.set('a') }, X::Parameter::InvalidConcreteness, 'SetHash.set';
throws-like { SetHash.unset('a') }, X::Parameter::InvalidConcreteness, 'SetHash.unset';
throws-like { BagHash.add('a') }, X::Parameter::InvalidConcreteness, 'BagHash.add';
throws-like { BagHash.remove('a') }, X::Parameter::InvalidConcreteness, 'BagHash.remove';
throws-like { SetHash.grab }, X::Multi::NoMatch,
    message => /'grab(SetHash:U: )'/, 'SetHash.grab names the type object';
throws-like { Array.pop }, X::Parameter::InvalidConcreteness,
    message => /"Invocant of method 'pop' must be an object instance of type"/,
    '... with the concreteness message';

{
    my SetHash $s;
    throws-like { $s.set('a') }, X::Parameter::InvalidConcreteness,
        'a SetHash-typed variable holding the type object';
    my Array $a;
    $a.push(1);
    is $a.raku, '$[1]', 'push still vivifies an Array-typed variable';
    my @x = 1, 2;
    is @x.pop, 2, 'a defined Array pops';
    my $sh = SetHash.new(<a>);
    $sh.set('b');
    is $sh.keys.sort.list, <a b>, 'a defined SetHash sets';
}
