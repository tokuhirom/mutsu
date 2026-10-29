use Test;

# A call with named arguments replays the plain-method lane keyed by the
# arguments' names and value types. Repeated calls of one site must still see
# each call's own names and values, and a shape the key cannot hold (a
# Junction value, a pair-valued argument) must still take the general path.

plan 20;

class C {
    method m(Int $x, Bool :$flag = True --> Int) { $flag ?? $x !! -$x }
    method slurpy(*%h) { %h.keys.sort.join(',') ~ '=' ~ %h.values.sort.join(',') }
    method pos(*@a, *%h) { @a.elems ~ '/' ~ %h.elems }
    method typed(Int :$n, Str :$s) { ($n // 'no-n') ~ '|' ~ ($s // 'no-s') }
    method opt(:$x) { $x.defined ?? "x=$x" !! 'none' }
    method only-x(:$x!) { $x }
}
my $c = C.new;

# one call site, values that differ call to call
my @got;
for True, False, True, False -> $f { @got.push: $c.m(5, :flag($f)) }
is-deeply @got, [5, -5, 5, -5], 'the named value is read on every call';

# names that differ call to call through a slurpy
is $c.slurpy(:a(1)), 'a=1', 'first name';
is $c.slurpy(:b(2)), 'b=2', 'a different name does not replay the first';
is $c.slurpy(:a(1), :b(2)), 'a,b=1,2', 'two named arguments';
is $c.slurpy(:b(2), :a(1)), 'a,b=1,2', 'the same two in the other order';
is $c.slurpy(), '=', 'none';

# value types that differ
is $c.typed(:n(3)), '3|no-s', 'an Int named value';
is $c.typed(:s('x')), 'no-n|x', 'a Str named value';
is $c.typed(:n(3), :s('x')), '3|x', 'both';
throws-like { $c.typed(:n('x')) }, X::TypeCheck::Binding::Parameter, 'a wrong-typed value is still refused';
throws-like { $c.typed(:n('y')) }, X::TypeCheck::Binding::Parameter, 'and refused on the replay';

# positional and named together
is $c.pos(1, 2, :a(3)), '2/1', 'positionals and a named argument';
is $c.pos(1, :a(3), :b(4)), '1/2', 'the counts follow the call';

# defined / undefined named values
is $c.opt(:x(5)), 'x=5', 'a defined value';
is $c.opt(:x(Int)), 'none', 'a type object value is not the defined one';
is $c.opt(), 'none', 'no argument';

# required named
is $c.only-x(:x(7)), 7, 'a required named argument';
throws-like { $c.only-x() }, Exception, 'missing it dies';

# a Junction value autothreads the call
my $j = $c.m(2, :flag(True | False));
ok $j ~~ Junction, 'a Junction named value still autothreads';

# a pair-valued named argument
is $c.opt(:x(1 => 2)).starts-with('x='), True, 'a pair as the named value';
