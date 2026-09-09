use Test;
use MONKEY-SEE-NO-EVAL;

plan 11;

# Whether a closure captures the WHOLE visible env by name is decided per
# compiled chunk, not by a process-global "some EVAL happened somewhere" latch
# (#7565). Every assertion below runs in a file that has already evaluated a
# string, so the latch is set and only the per-chunk determination decides.
is EVAL('1'), 1, 'the program has EVALed, so the reflective latch is set';

# A closure whose own body names a lexical dynamically still reaches its
# creating scope after escaping it -- one case per spelling the per-chunk scan
# has to recognise.
sub make-evaler() {
    my $secret = 'creation-scope';
    sub { EVAL '$secret' };
}
is make-evaler()(), 'creation-scope', 'an EVAL in a closure body reads the creating scope';

sub make-dereffer() {
    my $hidden = 'deref-scope';
    sub { $::('hidden') };
}
is make-dereffer()(), 'deref-scope', 'a symbolic deref in a closure body reads the creating scope';

sub make-outer() {
    my $o = 'outer-scope';
    sub { $OUTER::o };
}
is make-outer()(), 'outer-scope', 'an OUTER:: read in a closure body reads the creating scope';

# The need propagates outward: the OUTER closure escapes its creating scope and
# only then creates the reflective one, so it is the outer capture that has to
# be wide.
sub make-factory() {
    my $deep = 'deep-creation-scope';
    sub { sub { EVAL '$deep' } };
}
is make-factory()()(), 'deep-creation-scope',
    'a closure nested inside an escaping one still reads the outer creating scope';

# The ordinary closures of the same file take the narrow upvalue capture even
# though the latch is set, and must be unaffected by it.
sub make-counter() {
    my $n = 0;
    sub { $n = $n + 1; $n };
}
my $counter = make-counter();
$counter();
is $counter(), 2, 'a non-reflective escaping closure keeps its own free variable';

my @adders = (1, 2, 3).map(-> $k { sub ($x) { $x + $k } });
is @adders[0](10) ~ ',' ~ @adders[1](10) ~ ',' ~ @adders[2](10), '11,12,13',
    'each closure of a loop keeps its own captured value';

# A routine's registration marker (`__mutsu_callable_id::Pkg::name`) is no
# longer carried in the closure capture -- it is read from the live env. `state`
# scoping is keyed off it, so these pin that it is still found.
sub make-stateful() {
    sub { state $seen = 0; $seen = $seen + 1; $seen };
}
my $first = make-stateful();
my $second = make-stateful();
$first(); $first();
is $first(), 3, 'a closure state variable counts its own invocations';
is $second(), 1, 'a separate closure instance gets its own state variable';

# A closure body calling a routine imported into the file scope: the capture
# filter drops the lexical `&name`, so this resolves through the registry.
my $uses-import = sub { is 1 + 1, 2, 'a closure body calls an imported routine' };
$uses-import();

# The END-phaser refresh that runs on every closure return reads the live env
# key by key instead of flattening it; it must still observe the write.
our $watched = 'initial';
my $writer = sub { $watched = 'written-by-closure' };
$writer();

END {
    is $watched, 'written-by-closure', 'an END phaser sees the value a closure wrote';
}
