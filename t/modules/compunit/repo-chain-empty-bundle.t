use Test;

# The chain tail is reserved for the bundled batteries before anything knows
# whether the bundle has any (the scan runs on first use, #19197). A bundle
# with no distribution must still give a well-formed chain: nothing bundled is
# linked in, and with no Rakudo-Core there is no `core` Installation either.

plan 4;

my $empty = $*TMPDIR.add("mutsu-empty-bundle-$*PID");
$empty.mkdir;
LEAVE { $empty.rmdir if $empty.e }

sub run-code(Str $code) {
    my %env = %*ENV;
    %env<MUTSU_BUNDLE_DIR> = ~$empty;
    my $r = run($*EXECUTABLE, '-e', $code, :out, :err, :%env);
    my $out = $r.out.slurp(:close).trim;
    my $err = $r.err.slurp(:close).trim;
    $r.exitcode == 0 ?? $out !! "[exit {$r.exitcode}] $err"
}

# The default chain is the current directory and the site Installation; the
# bundled links and the `core` Installation would follow them.
my $chain = run-code('print $*REPO.repo-chain.map(*.path-spec).join(",")');
ok $chain.starts-with('file#.,inst#'), 'repo-chain answers the default links';
is $chain.split(',').elems, 2, 'nothing bundled and no core repository is linked in';

my $walk = run-code('my $r = $*REPO; my $n = 0; while $r { $n++; $r = $r.next-repo }; print $n == $*REPO.repo-chain.elems');
is $walk, 'True', 'a hand walk of .next-repo agrees with repo-chain';

is run-code('print try { require ::("JSON::Fast"); "found" } // "missing"'), 'missing',
    'a bundled module is not found without the bundle';
