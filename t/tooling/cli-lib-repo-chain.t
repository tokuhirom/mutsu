use Test;

plan 5;

# `-I` (and MUTSULIB) repositories head `$*REPO`'s chain, as Rakudo's `-I`
# and RAKULIB do, and a `file#PATH` repository spec names the same
# FileSystem repository as the bare PATH. App::Lorea's test forwards its own
# include path to a child process exactly this way:
#
#     constant INCLUDE = $*REPO.repo-chain.map: *.path-spec;
#     Proc::Async.new: $*EXECUTABLE, '-I' «~« INCLUDE, './bin/lorea', ...
#
# which failed because `-I` paths were missing from the chain and a
# `-Ifile#...` spec was taken as a literal directory name.

my $exe = $*EXECUTABLE;
my $lib = 't/fixtures/lib-precedence/plain';
my $abs = $lib.IO.absolute;

sub run-code(*@args) {
    my $r = run($exe, |@args, :out, :err);
    my $out = $r.out.slurp(:close).trim;
    my $err = $r.err.slurp(:close).trim;
    $r.exitcode == 0 ?? $out !! "[exit {$r.exitcode}] $err"
}

is run-code('-I', "file#$lib", '-e', 'use PrecProbe; print prec-probe-who()'),
    'plain', '-I accepts a file# repository spec';

is run-code("-Ifile#$lib", '-e', 'use PrecProbe; print prec-probe-who()'),
    'plain', 'the joined -Ifile#PATH form works too';

is run-code('-I', $lib, '-e', 'print $*REPO.repo-chain.head.path-spec'),
    "file#$abs", 'an -I directory heads $*REPO.repo-chain';

is run-code('-I', $lib, '-I', 't/fixtures', '-e',
        'print $*REPO.repo-chain.head(2).map(*.path-spec).join(",")'),
    "file#$abs,file#{'t/fixtures'.IO.absolute}", 'several -I paths keep their order';

# Round trip: a child started with the parent's chain as its -I list sees
# the same modules.
is run-code('-I', $lib, '-e',
        q:to/CODE/.subst("\n", ' ', :g)),
        my @inc = $*REPO.repo-chain.map(*.path-spec).grep(*.starts-with("file#"));
        my $p = run $*EXECUTABLE, "-I" «~« @inc, "-e", "use PrecProbe; print prec-probe-who()", :out;
        print $p.out.slurp(:close)
        CODE
    'plain', 'a forwarded repo-chain resolves modules in a child';
