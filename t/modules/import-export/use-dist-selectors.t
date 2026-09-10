use v6;
use Test;

plan 6;

# `use Module:auth<...>:ver<...>:api<...>` refines WHICH installed distribution
# is loaded when several provide the same short name (two JSON::Class dists
# exist on fez, by different authors). The selectors used to be parsed and
# DISCARDED, and the inst# repo scan returned whichever dist JSON read_dir
# happened to yield first. Now candidates are filtered by the selectors and the
# highest version wins. The fixture repo has two SelTest dists:
#   alice 0.0.6 api<0>  /  bob 0.0.21 api<1.0>

my $exe = $*EXECUTABLE;
my $repo = 'inst#t/fixtures/dist-selectors';

sub who(Str $use-stmt) {
    my $r = run($exe, '-I', $repo, '-e', $use-stmt ~ '; say sel-test-who()', :out, :err);
    $r.out.slurp(:close).trim ~ ($r.exitcode == 0 ?? '' !! ' [exit ' ~ $r.exitcode ~ ']')
}

is who('use SelTest:auth<zef:alice>'), 'alice-0.0.6', ':auth picks the matching dist';
is who('use SelTest:auth<zef:bob>'), 'bob-0.0.21', ':auth picks the other dist';
is who('use SelTest'), 'bob-0.0.21', 'no selector: the highest version wins';
is who('use SelTest:ver<0.0.14+>'), 'bob-0.0.21', ':ver<0.0.14+> excludes the older dist';
is who('use SelTest:ver<0.0.6>'), 'alice-0.0.6', 'an exact :ver picks the older dist';
is who('use SelTest:api<1.0>'), 'bob-0.0.21', ':api selects like a version';
