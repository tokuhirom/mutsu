use Test;

plan 4;

# Binding a Hash with a role mixed in to a `%` attribute keeps the role and
# the Hash itself (Memoize: `:CACHE(my %hash does Flush)`, then `.?FLUSH`).

my $flushed = 0;
role Flush { method FLUSH() { ++$flushed } }
my role M { has %.cache; method set(\c) { %!cache := c; self } }

sub g(:%CACHE!) { M.new.set(%CACHE) }
my $o = g(:CACHE(my %h does Flush));
is $o.cache.^name, 'Hash+{Flush}', 'the bound attribute keeps the role';
$o.cache.?FLUSH;
is $flushed, 1, 'and its method';
%h<k> = 1;
is-deeply $o.cache.keys.List, ('k',), 'the attribute is the same Hash';

class C { has %.c; method s(\x) { %!c := x; self } }
my %b does Flush;
is C.new.s(%b).c.^name, 'Hash+{Flush}', 'a class attribute too';
