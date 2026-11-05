use v6;
use Test;

# `CompUnit::Repository::Installation.install` takes a distribution that
# reaches it through a container (zef passes `$candidate.dist`, an `is rw`
# accessor result) exactly as a bare one.
plan 4;

my $src = $*TMPDIR.add('inst-wrap-src-' ~ $*PID);
my $prefix = $*TMPDIR.add('inst-wrap-repo-' ~ $*PID);
END { run 'rm', '-rf', ~$src, ~$prefix if $src.e || $prefix.e }
$src.add('lib').mkdir;
$prefix.mkdir;
$src.add('lib/WrapDist.rakumod').spurt: "unit module WrapDist;\n";
$src.add('META6.json').spurt: '{"name":"WrapDist","ver":"1","provides":{"WrapDist":"lib/WrapDist.rakumod"}}';

my $repo = CompUnit::Repository::Installation.new(:prefix($prefix));
my $dist = Distribution::Path.new($src);

class Holder { has $.dist is rw }
my $holder = Holder.new(:$dist);

ok $repo.install($holder.dist), 'a distribution read from an `is rw` attribute installs';
is $prefix.add('dist').dir.elems, 1, 'one distribution record was written';
is $prefix.add('sources').dir.elems, 1, 'and its source file was copied';

my @list = $dist;
my $again = $prefix.add('again');
$again.mkdir;
ok CompUnit::Repository::Installation.new(:prefix($again)).install(@list[0]),
    'a distribution taken from an array element installs';
