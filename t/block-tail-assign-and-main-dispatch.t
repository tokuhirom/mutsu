use v6;
use Test;

plan 8;

# --- A block-final assignment to an existing variable carries its value ---
# `.map({ $x = 9 })` must yield the assigned value, not the topic `$_`.
my $x;
is-deeply (1, 2, 3).map({ $x = 9 }).List, (9, 9, 9),
    'map block ending in $x = 9 yields the assigned value';

# The anonymous scalar `$` assigned in a map block carries its value too.
is-deeply <a b c>.map({ $ = "X" ~ $_ }).List, ("Xa", "Xb", "Xc"),
    'map block { $ = ... } yields the assigned anonymous-scalar value';

# do-block parity (unchanged behavior).
my $y;
is (do { $y = 42 }), 42, 'do { $y = 42 } yields 42';

# A grep block ending in an assignment is truthy-tested on the assigned value.
my $z;
is-deeply (1, 2, 3).grep({ $z = $_ %% 2 }).List, (2,),
    'grep block ending in $z = ... filters on the assigned value';

# --- MAIN multi-dispatch: body errors propagate, binding rejection falls through ---
my $prog = $*TMPDIR.add("main-dispatch-{$*PID}.raku");
sub run-prog($src, *@args) {
    $prog.spurt: $src;
    my $proc = run($*EXECUTABLE, $prog, @args, :out, :err);
    my $out = $proc.out.slurp(:close).chomp;
    my $err = $proc.err.slurp(:close);
    ($out, $err);
}

# Literal-subcommand fallthrough: a non-matching leading literal must reach the
# next candidate rather than error out.
my $lit-multi = q:to/RAKU/;
    multi sub MAIN('go', Str $x) { say "go $x" }
    multi sub MAIN('stop')       { say "stop" }
    RAKU

is run-prog($lit-multi, 'go', 'now')[0], 'go now',
    'MAIN dispatch matches the go subcommand';
is run-prog($lit-multi, 'stop')[0], 'stop',
    'MAIN dispatch falls through to the stop subcommand';

# A matched candidate whose body throws a (non-return) type-check error must
# surface that error, NOT mask it as a "Usage" dispatch failure — the bug that
# made `zef install` print Usage instead of the real error.
my ($body-out, $body-err) = run-prog(
    q:to/RAKU/, 'go');
    multi sub MAIN('go') { my Int $x = "not-an-int" }
    RAKU
like $body-err, /'Type check failed'/,
    'MAIN body type-check error propagates instead of printing Usage';

# A genuine no-match still prints Usage.
my ($nomatch-out, $nomatch-err) = run-prog(
    q:to/RAKU/, 'nope');
    multi sub MAIN('go') { say "go" }
    RAKU
like $nomatch-err, /Usage/, 'a genuine no-match still prints Usage';

$prog.unlink;
