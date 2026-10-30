use Test;

plan 11;

# Rakudo-targeting tools (Pakku's test/build children) hand their repository
# chain to a child interpreter through RAKULIB: a comma-separated list of
# `inst#PATH`, `file#PATH` or bare-path specs (mutsu#14918). It is searched
# right after MUTSULIB, so `-I` and MUTSULIB entries win.

my $exe = $*EXECUTABLE;
my $base = 't/fixtures/lib-precedence'.IO.absolute;
my $plain = "$base/plain";
my $envdir = "$base/env";
my $inst = "$base/inst";
my $code = 'use PrecProbe; print prec-probe-who()';

sub child(%vars, *@args) {
    my %env = %*ENV;
    %env<RAKULIB MUTSULIB>:delete;
    %env{.key} = .value for %vars;
    my $r = run($exe, |@args, :out, :err, :%env);
    my $out = $r.out.slurp(:close).trim;
    my $err = $r.err.slurp(:close).trim;
    $r.exitcode == 0 ?? $out !! "[exit {$r.exitcode}] $err"
}

is child({RAKULIB => $plain}, '-e', $code), 'plain', 'a bare path in RAKULIB is searched';
is child({RAKULIB => "file#$plain"}, '-e', $code), 'plain', 'a file# spec is searched';
is child({RAKULIB => "inst#$inst"}, '-e', $code), 'installed', 'an inst# spec is searched';
is child({RAKULIB => "$envdir,$plain"}, '-e', $code), 'env', 'RAKULIB is comma-separated, first entry wins';
is child({RAKULIB => "$plain,$envdir"}, '-e', $code), 'plain', 'listed order decides';
like child({RAKULIB => "$envdir:$plain"}, '-e', $code), /^'[exit '/,
    'a colon is not a separator (the whole value is one path)';
like child({RAKULIB => "bogus#$plain"}, '-e', $code), /^'[exit '/,
    'an entry with an unknown scheme is ignored';
is child({RAKULIB => "CompUnit::Repository::Staging#name(site)#$inst"}, '-e', $code), 'installed',
    'a staging repository path-spec (what Pakku hands its children) is an installation';
is child({RAKULIB => "CompUnit::Repository::Installation#$inst"}, '-e', $code), 'installed',
    'a spelled-out Installation spec is an installation';
is child({RAKULIB => $plain, MUTSULIB => $envdir}, '-e', $code), 'env', 'MUTSULIB wins over RAKULIB';
is child({RAKULIB => $envdir}, '-I', $plain, '-e', $code), 'plain', '-I wins over RAKULIB';

done-testing;
