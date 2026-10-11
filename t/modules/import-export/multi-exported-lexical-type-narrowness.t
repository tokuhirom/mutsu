use v6;
use Test;

# An exported multi whose candidate is typed with a lexical class (`my class
# VL`) the MODULE imported must rank that candidate against the real type,
# even when the calling script never imported `VL` itself. The candidate is
# found applicable either way; it used to lose the narrowness ranking to an
# `@_` candidate because `VL` was resolved in the caller's scope (#12710).

plan 4;

my $lib = $*TMPDIR.add("mutsu-multi-lexical-narrowness-{$*PID}");
$lib.mkdir;
LEAVE { try { .unlink for $lib.dir; $lib.rmdir } }

$lib.add('VLm12710.rakumod').spurt(q:to/END/);
    my class VL does Positional { }
    my sub EXPORT() { Map.new: (VL => VL) }
    END

$lib.add('Gm12710.rakumod').spurt(q:to/END/);
    use VLm12710;
    my proto sub g(|) is export {*}
    my multi sub g(VL:D $_) { "VL" }
    my multi sub g(@_) { "array" }
    sub mk() is export { VL.new }
    END

sub run-script(Str $code) {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e', $code, :out, :err);
    $proc.out.slurp(:close).trim ~ $proc.err.slurp(:close).trim
}

is run-script('use Gm12710; say g(mk())'), 'VL',
    'lexical-class candidate out-narrows @_ when the caller did not import the class';
is run-script('use Gm12710; use VLm12710; say g(mk())'), 'VL',
    'and still does when the caller imported it too';
is run-script('use Gm12710; say g([1, 2])'), 'array',
    'a plain Array still reaches the @_ candidate';
is run-script('use Gm12710; say g(mk()) for ^2'), "VL\nVL",
    'repeated calls rank the same';
