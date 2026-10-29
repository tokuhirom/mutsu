use v6;
use Test;

plan 12;

# A method candidate's capture carries the invocant first
# (sourcery's `$candidate.cando(\(Str, ...))`).
my @numeric = Str.^can('Numeric');
my @cands = @numeric.map(*.candidates.Slip);

is @cands.elems, 3, 'Str.^can("Numeric") offers three candidates';
is @cands.grep(*.cando(\(Str))).elems, 2, 'the type-object invocant matches the :U candidates';
is @cands.grep(*.cando(\("x"))).elems, 1, 'a defined Str matches the Str:D candidate only';
is @cands.grep(*.cando(\())).elems, 0, 'a capture without an invocant matches no method';

# A built-in candidate answers for itself, not for its family.
my $str-d = @cands.first({ .signature.raku.contains('Str:D') });
is $str-d.cando(\("x")).elems, 1, 'the Str:D candidate matches a defined Str';
is $str-d.cando(\(Str)).elems, 0, 'and not the type object';

# The dispatcher Method answers with the candidates that fit.
is @numeric.map({ .cando(\("x")).elems }).sum, 1, 'dispatcher cando with a defined Str';

# `cando` asks what a signature accepts: `Int(Cool) $base` takes any Cool.
sub f(Int(Cool) $b) { 1 }
is &f.cando(\("beer")).elems, 1, 'a coercion parameter accepts a Cool that would not coerce';
is &f.cando(\(Any)).elems, 0, 'but not an Any';

my @base = 42.^can('base').map(*.candidates.Slip);
is @base.grep(*.cando(\(42, "beer"))).elems, 2, '42.base("beer") fits two candidates';
is @base.grep(*.cando(\(42, 16))).elems, 2, '42.base(16) fits two candidates';

# User multis keep working.
multi g(Int $a) { 1 }
multi g(Str $a) { 2 }
is &g.candidates.map({ .cando(\(5)).elems }).sum, 1, 'user multi candidates answer for themselves';
