use Test;

# #11276: `Failure.Capture` and `Signature.Capture`, as Rakudo declares them.
# `X::Cannot::Capture` names the value by its `.gist` (a handled Failure's is
# its `.mess`); an unhandled Failure throws its own exception.

plan 8;

my $f = Failure.new("x");
throws-like { $f.Capture }, X::AdHoc, message => 'x', 'an unhandled Failure throws its exception';

my $h = Failure.new("x");
$h.so;
throws-like { $h.Capture }, X::Cannot::Capture,
    message => /^ 'Cannot unpack or Capture `(HANDLED) x' \n '`.' /, 'a handled one names its .mess';

my $n = Failure.new(X::NYI.new(feature => "f"));
$n.so;
is $n.mess, "(HANDLED) f not yet implemented. Sorry.\n", '.mess renders a built-in exception\'s message';
throws-like { $n.Capture }, X::Cannot::Capture,
    message => /'`(HANDLED) f not yet implemented. Sorry.'/, '...and so does Capture';

throws-like { Failure.Capture }, X::Cannot::Capture,
    message => /^ 'Cannot unpack or Capture `(Failure)`.'/, 'the Failure type object';
throws-like { :($a).Capture }, X::Cannot::Capture,
    message => /^ 'Cannot unpack or Capture `($a)`.'/, 'a Signature';
throws-like { Signature.Capture }, X::Cannot::Capture,
    message => /^ 'Cannot unpack or Capture `(Signature)`.'/, 'the Signature type object';
throws-like { :($a).Capture }, X::Cannot::Capture,
    message => /'To create a Capture, add parentheses'/, 'with rakudo\'s explanation';
