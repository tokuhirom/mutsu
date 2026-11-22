use Test;

# #18747: an undeclared variable inside `EVAL` is offered the lexicals of the
# scope calling `EVAL` as "Did you mean" suggestions, as rakudo does.

plan 4;

my $ab;
try EVAL q[say $ac];
is $!.message, "Variable '\$ac' is not declared. Did you mean '\$ab'?", 'a caller scalar';

my @foo;
try EVAL q[say $foo];
is $!.message, "Variable '\$foo' is not declared. Did you mean '\@foo'?", 'a caller array';

sub f { my $inner = 1; try EVAL q[say $innr]; $!.message }
is f(), "Variable '\$innr' is not declared. Did you mean '\$inner'?", 'a routine lexical';

try EVAL q[say $qqqzzz];
is $!.message,
    "Variable '\$qqqzzz' is not declared. Perhaps you forgot a 'sub' if this\nwas intended to be part of a signature?",
    'no near name: the forgotten-sub hint';
