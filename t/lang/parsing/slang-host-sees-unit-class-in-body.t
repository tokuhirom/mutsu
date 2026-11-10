use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang action that compiles its block (`meta-object`) while the unit is being
# parsed does not judge a class the unit declared above it undeclared.
use MetaObjectBlockSlang;

plan 2;

class Widget { has $.size = 3; method area { $!size ** 2 } }

my $f = mk { Widget.new(size => 4).area };
is $f(), 16, 'a block naming a class declared in the same file compiles';
is (mk { Widget.new.area })(), 9, 'also as a term';
