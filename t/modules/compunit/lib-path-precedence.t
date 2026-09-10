use v6;
use Test;

plan 9;

# Module search paths form ONE precedence chain, walked in order:
#   `use lib` (newest first) -> `-I` (in order) -> MUTSULIB -> installed repos.
# The resolver used to hoist every `inst#` (CompUnit::Repository::Installation)
# entry to the front of that walk, and the default site repository was registered
# by `Interpreter::new` before any `-I` path was added, so an installed module
# shadowed an explicit `-I` -- the one thing the flag exists to prevent. That
# silently invalidated every measurement taken with `-I` on a machine that had
# run `mzef install` (see docs/batteries and the DBIish survey).
#
# The fixture repo holds three copies of PrecProbe, each reporting where it came
# from: a plain directory, a second plain directory (for MUTSULIB), and an
# installed distribution whose version (9.9.9) is higher than anything else --
# `-I` is not a version hint, so it must win anyway.

my $exe   = $*EXECUTABLE;
my $base  = 't/fixtures/lib-precedence';
my $plain = "$base/plain";
my $env   = "$base/env";
my $inst  = "inst#$base/inst";

sub who(*@args, Str :$code = 'use PrecProbe; say prec-probe-who()') {
    my $r = run($exe, |@args, '-e', $code, :out, :err);
    my $out = $r.out.slurp(:close).trim;
    my $err = $r.err.slurp(:close).trim;
    $r.exitcode == 0 ?? $out !! "$out [exit {$r.exitcode}] $err"
}

is who('-I', $plain), 'plain', 'a plain -I directory is found';
is who('-I', $inst), 'installed', 'an -I installed repository is found';

is who('-I', $plain, '-I', $inst), 'plain',
    '-I beats an installed repository listed after it';
is who('-I', $inst, '-I', $plain), 'installed',
    'an installed repository listed first still wins';

is who('-I', $plain, '-I', $env), 'plain', 'the first -I wins over the second';
is who('-I', $env, '-I', $plain), 'env', 'the first -I wins over the second (reversed)';

{
    temp %*ENV<MUTSULIB> = $env;
    is who('-I', $plain), 'plain', '-I beats MUTSULIB';
    is who(), 'env', 'MUTSULIB alone is still searched';
}

# `use lib` unshifts onto the repository chain, so it outranks -I.
is who('-I', $plain, code => "use lib '$env'; use PrecProbe; say prec-probe-who()"),
    'env', 'use lib takes precedence over -I';
