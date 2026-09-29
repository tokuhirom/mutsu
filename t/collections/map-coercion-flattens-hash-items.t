use Test;

plan 6;

# Test::Describe: `sub EXPORT(--> Map()) { Foo::, "&x" => &x }` -- a non-itemized
# Hash/Map/Stash item of a list contributes its pairs to a hash initializer.
my %h = a => 1;
is (%h, "x" => 3).Map.keys.sort.join(','), 'a,x', 'List.Map flattens a Hash item';
is (%h, "x" => 3, "y" => 4).Hash.keys.sort.join(','), 'a,x,y', 'List.Hash flattens a Hash item';

sub g(--> Map()) { %h, "x" => 3 }
is g().keys.sort.join(','), 'a,x', '--> Map() flattens a Hash item';

module Foo { our sub z { 1 }; our $v = 2 }
is (Foo::, "x" => 3).Map.keys.sort.join(','), '$v,&z,x', 'List.Map flattens a package stash';
sub s(--> Map()) { Foo::, "x" => 3 }
is s().keys.sort.join(','), '$v,&z,x', '--> Map() flattens a package stash';
dies-ok { (%h, "x").Map }, 'a genuine odd element still dies';
