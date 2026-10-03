use Test;

plan 3;

# Binding a named-parameter destructure list to a Capture reads the
# Capture's named part.
my (:$a, :@b) := \(:a(5), :b([6, 7]));
is $a, 5, 'scalar named binds from the Capture';
is @b.join(','), '6,7', 'array named binds from the Capture';

my (:$c) := \(:c<x>);
is $c, 'x', 'single named destructure';
