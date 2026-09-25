use Test;

# The bundled batteries' links on `$*REPO`'s chain are built lazily, on the
# first read of the chain tail's `next-repo`, so a `say "hello"` does not pay
# for them. Here the very first touch of the chain is a hand walk over
# `.next-repo` (not `resolve` or `repo-chain`), and it must still see them.

plan 3;

sub spec($name) { CompUnit::DependencySpecification.new(:short-name($name)) }

my @walked;
my $r = $*REPO;
while $r {
    @walked.push: $r;
    $r = $r.next-repo;
}

ok @walked.first({ .candidates(spec('JSON::Fast')).elems }).defined,
    'the first hand walk of .next-repo reaches the bundled links';
is $*REPO.repo-chain.elems, @walked.elems,
    'repo-chain agrees with the hand walk';
is $*REPO.repo-chain.elems, @walked.elems,
    'the bundled links are linked in only once';
