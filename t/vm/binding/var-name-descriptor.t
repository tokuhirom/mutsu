use v6;
use Test;

plan 12;

# Container-descriptor `.VAR.name` (rakudo-verified matrix, 2026-08-12):
# a `my @x` declaration names its fresh container "@x", and the name travels
# with the container through binding chains (named-arg forward, slurpy
# re-flatten) — it is NOT reconstructed from call-site syntax. Text::CSV's
# `method CSV` gates its whole out/headers defaulting on
# `@kh.VAR.name ne "element"` after two slurpy hops (rakudo#2483 workaround).

my @x = 1, 2;
my @y := @x;
my @z = @x;
my %hh;

is @x.VAR.name, '@x', 'declared array reports its own name';
is @y.VAR.name, '@x', 'binding keeps the original container name (first name wins)';
is @z.VAR.name, '@z', 'assignment copies into a fresh container with its own name';
is %hh.VAR.name, '%hh', 'declared hash reports its own name';

sub f(:@kh) { @kh.VAR.name }
sub g(:@kh is copy) { @kh.push: 99; @kh.VAR.name }
sub h(*%args) { f(|%args) }

is f(), 'element', 'unsupplied @-param binds a fresh "element" container';
is f(kh => @x), '@x', 'named @-param aliasing a caller array reports the caller name';
is f(kh => my @w), '@w', 'inline-declared argument reports its declared name';
is g(kh => @x), 'element', 'is copy param owns a fresh "element" container';
is-deeply @x, [1, 2], 'is copy did not mutate the caller array';
is h(kh => @x), '@x', 'the name survives a slurpy re-flatten hop';

# The Text::CSV shape: two slurpy hops and a method boundary.
class C {
    method CSV(:$out! is copy, :@kh) { @kh.VAR.name }
    method csv(*%args) { self.CSV(:out(%args<out> // Any), |%args) }
}
sub csv(*%args) { C.csv(|%args) }
is csv(kh => my @kh), '@kh', 'the name survives two slurpy hops into a method';
is csv(), 'element', 'unsupplied stays "element" through the same chain';
