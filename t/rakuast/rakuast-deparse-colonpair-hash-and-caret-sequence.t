use Test;
use experimental :rakuast;

# `.DEPARSE` of a left-exclusive sequence inside parentheses and of colonpairs
# inside a hash literal: both keep the spelling the program wrote. The expected
# strings are what `raku` prints.

my @cases =
  'my @a = (1, 2 ^... 10);' => "my \@a = (1, 2 ^... 10)\n",
  'my @a = (1, 2 ^... *);' => "my \@a = (1, 2 ^... *)\n",
  'my @a = (1 ^...^ 10);' => "my \@a = (1 ^...^ 10)\n",
  'my @a = 1 ^... 10;' => "my \@a = 1 ^... 10\n",
  'my %h = {:2b, :1c};' => "my \%h = \{:2b, :1c}\n",
  'my %h = {:b(2)};' => "my \%h = \{:b(2)}\n",
  'my %h = {:!b, :c};' => "my \%h = \{:!b, :c}\n",
  'my %h = {:b<x>, d => 1};' => "my \%h = \{:b<x>, d => 1}\n",
  'my %h = %(:2b, :1c);' => "my \%h = \%(:2b, :1c)\n",
  'my $x = {a => 1, :2b};' => "my \$x = \{a => 1, :2b}\n";

plan @cases + 2;

for @cases -> $case {
    is $case.key.AST.DEPARSE, $case.value, $case.key;
}

# The values still behave: the wrapped colonpair values execute as before.
my %h = {:2b, :!c, :d<x y>, :e(5), :z};
is-deeply %h.sort.map({ .key => .value.raku }).List,
    (:b("2"), :c("Bool::False"), :d('$("x", "y")'), :e("5"), :z("Bool::True")).List,
    'hash literal colonpair values';
is-deeply (1, 2 ^... 5).List, (2, 3, 4, 5), 'left-exclusive sequence in parens';
