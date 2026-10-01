unit module ModuleLexicalRwArg;

# Module-level `my` variables handed to `is rw` parameters by the module's own
# routines (#10372).
my Str $typed;
my $plain;
my $counter = 0;

our sub set-typed(Str $f is rw, Str $v) is export { $f = $v }
sub set-plain($x is rw) { $x = 42 }
sub bump($n is rw) { $n++ }

# The call is the whole body: the routine runs as a TRIR chunk.
our sub fill-typed() is export { set-typed($typed, 'opened') }
our sub show-typed() is export { $typed.defined ?? $typed !! $typed.^name }

our sub fill-plain() is export { set-plain($plain); $plain }
our sub show-plain() is export { $plain }

our sub bump-twice() is export { bump($counter); bump($counter); $counter }
