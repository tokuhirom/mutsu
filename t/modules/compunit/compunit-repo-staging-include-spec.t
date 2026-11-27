use v6;
use Test;

# zef's test phase runs a staged distribution's test files as
# `$*EXECUTABLE -I <staging-repo.path-spec> t/foo.t`, and a staging repository's
# `path-spec` spells its class out: `CompUnit::Repository::Staging#name(site)#PATH`.
# `-I` must take that spelling (as `RAKULIB` does) as the installation at PATH,
# or the distribution's own tests cannot `use` it (#19246).
plan 4;

my $src    = $*TMPDIR.add('staging-inc-src-' ~ $*PID);
my $prefix = $*TMPDIR.add('staging-inc-repo-' ~ $*PID);
END { run 'rm', '-rf', ~$src, ~$prefix if $src.e || $prefix.e }
$src.add('lib').mkdir;
$prefix.mkdir;
$src.add('lib/StagedIncDist.rakumod').spurt:
    "unit module StagedIncDist;\nsub staged-inc() is export \{ 'found' \}\n";
$src.add('META6.json').spurt:
    '{"name":"StagedIncDist","ver":"1","provides":{"StagedIncDist":"lib/StagedIncDist.rakumod"}}';

CompUnit::Repository::Installation.new(:prefix($prefix)).install(Distribution::Path.new($src));

sub use-with(*@args) {
    my $proc = run $*EXECUTABLE.absolute, |@args,
        '-e', 'use StagedIncDist; print staged-inc()', :out, :err;
    $proc.out.slurp(:close)
}

is use-with('-I', "CompUnit::Repository::Staging#name(site)#{$prefix.absolute}"), 'found',
    '-I with a spelled-out Staging spec loads from the installation';
is use-with("-ICompUnit::Repository::Staging#name(site)#{$prefix.absolute}"), 'found',
    'the joined -ISPEC form too';
is use-with('-I', "CompUnit::Repository::Installation#{$prefix.absolute}"), 'found',
    'a spelled-out Installation spec without a name';
is use-with('-I', "inst#{$prefix.absolute}"), 'found',
    'the short inst# spec still works';
