use v6;
use Test;

# Postcircumfix {} with no leading whitespace binds as a hash subscript on
# sub-call results and parenthesized expressions (raku semantics).
# Regression pins for todo/tickets/brace-subscript-after-call-and-parens.md:
# `routes{'/'}` (Humming-Bird) and `($a // $b){$key}` (Cro::HTTP::Router).

plan 9;

enum HTTPMethod <GET POST>;
sub local-routes() { { '/' => { GET => 'get-handler' } } }

is local-routes{'/'}{GET}, 'get-handler', 'brace subscript on no-paren sub call result chains';
is local-routes(){'/'}{GET}, 'get-handler', 'brace subscript on parenized sub call result chains';

# The imported-sub path goes through the listop call parser, which must also
# treat a no-whitespace `{` as a subscript (the Humming-Bird `routes{'/'}` shape).
use lib 't/lib';
use BraceSubscriptRoutes;
is routes{'/'}{'GET'}, 'imported-handler', 'imported sub call result subscripts';

my %a = x => 1;
my %b;
# NB: an empty hash is still defined, so `//` yields its LHS: (%b // %a){'x'}
# is %b{'x'} (Any) in raku too. Use the defined-operand shapes below.
is (%a // %b){'x'}, 1, 'brace subscript on parenthesized // expression';
nok ((%b // %a){'x'}).defined, '// yields defined LHS; subscript miss is Any (raku parity)';

my %h = a => 42;
is (True ?? %h !! %b){'a'}, 42, 'brace subscript on parenthesized ternary';

# .List on the subscript result (the exact Cro::HTTP::Router shape)
my %cfg = k => (1, 2);
is-deeply (%cfg // %b){'k'}.List, (1, 2), 'subscript result method call chains (Router.pm6:188 shape)';

# Whitespace before the brace still means a block/hash argument, not a subscript.
sub takes-hash(%h) { %h<key> }
is takes-hash({ key => 'v' }), 'v', 'explicit hash arg in parens still works';

# A statement block after a parenthesized condition keeps working.
my $side = 0;
if (1) { $side = 1 }
is $side, 1, 'if with parenthesized condition and spaced block unaffected';

done-testing;
