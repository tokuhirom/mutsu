use Test;
use experimental :rakuast;

# An anonymous `$` among several parameters of a pointy block deparses as `$`,
# not as the parser's internal name. The expected text is what `raku` prints.

my @cases =
  'my $f = -> $, $v { 1 };' => "my \$f = -> \$, \$v \{\n    1\n}\n",
  'my $f = -> $, $, $z { 1 };' => "my \$f = -> \$, \$, \$z \{\n    1\n}\n",
  'my $f = -> $a, $ { 1 };' => "my \$f = -> \$a, \$ \{\n    1\n}\n",
  'my $f = -> $ { 1 };' => "my \$f = -> \$ \{\n    1\n}\n";

plan @cases + 1;

for @cases -> $case {
    is $case.key.AST.DEPARSE.chomp ~ "\n", $case.value.chomp ~ "\n", $case.key;
}

my $f = -> $, $v { $v * 2 };
is $f(1, 21), 42, 'the block still runs';
