use v6;
use Test;

# A signature with no parameters but a return type keeps the ` --> ` arrow
# spaced: it renders as `( --> Int)` / `:( --> Int)`, not `(--> Int)`.
# From raku-doc Language/signatures.rakudoc (doc-diff findings [2]/[3]).

plan 6;

sub foo() of Int { 42 }
is &foo.signature.gist, '( --> Int)',  'return-only signature gist has spaced arrow';
is &foo.signature.raku, ':( --> Int)', 'return-only signature raku has spaced arrow';

my Int sub bar { 1 }
is &bar.signature.gist, '( --> Int)',  'my Int sub: gist spaced arrow';

# A parameterized signature is unaffected (space was already present).
sub baz($x --> Int) { 1 }
is &baz.signature.gist, '($x --> Int)', 'parameterized + return arrow unchanged';

# No return type: no arrow, no spurious space.
sub qux() { 1 }
is &qux.signature.gist, '()', 'empty signature, no return type';

sub quux($x) { 1 }
is &quux.signature.gist, '($x)', 'param-only signature unchanged';
