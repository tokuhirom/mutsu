use v6;
use Test;

# `.content` of an installed distribution's resource answers a handle on the
# file the install stored under the repository's `resources/` directory
# (#17513: Red reads its migration bundle from an installed dependency this way,
# and the handle pointed at `<prefix>/<id>` instead).
plan 4;

my $src = $*TMPDIR.add('inst-res-src-' ~ $*PID);
my $prefix = $*TMPDIR.add('inst-res-repo-' ~ $*PID);
END { run 'rm', '-rf', ~$src, ~$prefix if $src.e || $prefix.e }
$src.add('lib').mkdir(:parents);
$src.add('resources/red').mkdir(:parents);
$src.add('lib/ResDist.rakumod').spurt: "unit module ResDist;\n";
$src.add('resources/red/index.json').spurt: '{"format":1}';
$src.add('META6.json').spurt: '{"name":"ResDist","ver":"1","provides":{"ResDist":"lib/ResDist.rakumod"},'
    ~ '"resources":["red/index.json"]}';

my $repo = CompUnit::Repository::Installation.new(:prefix($prefix));
$repo.install(Distribution::Path.new($src), :!precompile);
my $dist = $repo.installed.first({ .meta<name> eq 'ResDist' });
my $handle = $dist.content('resources/red/index.json');
ok $handle.path.f, 'the handle names an existing file';
is $handle.path.parent.basename, 'resources', 'stored under the repository resources directory';
is $handle.path.parent.parent.absolute, $prefix.absolute, 'inside the repository prefix';
$handle.open;
is $handle.slurp, '{"format":1}', 'its content is the resource';
