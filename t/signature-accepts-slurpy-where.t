use Test;

plan 6;

# Signature.ACCEPTS (and `~~ Signature`) must evaluate a `where` constraint
# attached to a slurpy positional parameter against the *whole assembled
# array*, the same way real call binding does. Cro::HTTP::Router relies on
# this for its multi-candidate route matching
# (`$handler.signature.ACCEPTS($cap)`); a signature that always accepted
# meant a non-matching route candidate looked like a match, which sent the
# router down a request-handling path that never emitted a response and
# hung the whole request.

my $sig = :(*@path where *[*-1].ends-with('.html'));

nok $sig.ACCEPTS(\('foo', 'bar.jpg')), "slurpy where-constraint rejects a non-matching capture";
ok  $sig.ACCEPTS(\('foo', 'bar.html')), "slurpy where-constraint accepts a matching capture";

nok (\('foo', 'bar.jpg') ~~ $sig), "smart-match form agrees on the mismatch";
ok  (\('foo', 'bar.html') ~~ $sig), "smart-match form agrees on the match";

# Binding through a real call already worked; ACCEPTS must agree with it.
sub f(*@path where *[*-1].ends-with('.html')) { "matched" }
dies-ok { f('foo', 'bar.jpg') }, "real binding still rejects the mismatch";
lives-ok { f('foo', 'bar.html') }, "real binding still accepts the match";
