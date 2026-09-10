use v6;
use Test;

# Raku sinks the mainline's final statement value. When the tail is a fresh
# rvalue (a method call or `EVAL`) whose value is an unhandled Failure, sinking
# it throws. A container-wrapped tail (a bare variable or an assignment) does
# NOT trip. Bare *sub* calls are deliberately excluded (mutsu's soft-Failure
# modeling of a routine return diverges from Raku, so tripping them would throw
# where Raku does not). These are checked by running child mutsu processes and
# inspecting their exit status.

plan 10;

my $mutsu = $*EXECUTABLE.absolute;

sub run-code(Str $code) {
    my $proc = run $mutsu, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    { :$out, :err($err), exit => $proc.exitcode };
}

# --- fresh-rvalue tails: an unhandled Failure at program end throws ---

{
    my %r = run-code('my @a; @a.pop');
    isnt %r<exit>, 0, 'bare .pop on empty Array as tail throws';
    like %r<err>, /'empty'/, '.pop tail error mentions empty';
}

{
    my %r = run-code('my @a; @a.shift');
    isnt %r<exit>, 0, 'bare .shift on empty Array as tail throws';
}

# A bare method call whose value is a Failure trips (e.g. a coercion failure).
{
    my %r = run-code('"abc".Int');
    isnt %r<exit>, 0, 'a failing coercion method call as tail throws';
}

{
    my %r = run-code('my @a; { @a.pop }');
    isnt %r<exit>, 0, 'bare block whose tail is a Failure throws';
}

{
    my %r = run-code('do { my @a; @a.pop }');
    isnt %r<exit>, 0, 'do block whose tail is a Failure throws';
}

# --- container-wrapped tails: no throw (matches Raku) ---

{
    my %r = run-code('my @a; my $x = @a.pop');
    is %r<exit>, 0, 'assigning the Failure to a variable does not throw';
}

{
    my %r = run-code('my @a; my $x = @a.pop; my $y = $x');
    is %r<exit>, 0, 'a bare variable holding a Failure does not throw';
}

{
    my %r = run-code('sub f { my @a; @a.pop }; my $y = f()');
    is %r<exit>, 0, 'assigning a sub-returned Failure does not throw';
}

# --- EVAL as an rvalue keeps the Failure (does not throw inside EVAL) ---

{
    my %r = run-code('my $x = EVAL "my \@a; \@a.pop"; say "ok"');
    is %r<exit>, 0, 'EVAL as rvalue returns the Failure without throwing';
}
