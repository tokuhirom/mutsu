use Test;
use experimental :rakuast;

# `die(...)` with the parenthesis right after the name is a call over its
# arguments; with a space it is the listop over a parenthesised argument. The expected text is what `raku` prints.

my @cases =
  'die("S");' => "die(\"S\")\n",
  'die ("S");' => "die (\"S\")\n",
  'die("S", "T");' => "die(\"S\", \"T\")\n",
  'die "S";' => "die \"S\"\n",
  'die("S") if 1;' => "die(\"S\") if 1\n",
  'my $x = die("S");' => "my \$x = die(\"S\")\n",
  'fail "S";' => "fail \"S\"\n";

plan @cases + 3;

for @cases -> $case {
    is $case.key.AST.DEPARSE.chomp ~ "\n", $case.value, $case.key;
}

# The calls still die with the same message.
throws-like { die("boom") }, X::AdHoc, message => 'boom', 'die(...) dies';
throws-like { die("a", "b") }, X::AdHoc, message => 'ab', 'die(a, b) joins its arguments';
sub f { fail("nope") }
is f().exception.message, 'nope', 'fail(...) fails';
