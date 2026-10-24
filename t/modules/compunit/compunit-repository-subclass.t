use v6;
use Test;
use CompUnit::Repository::Staging;

# A user class may derive from `CompUnit::Repository::Installation`, as
# rakudo's own `CompUnit::Repository::Staging` does (mutsu#14914), and
# `.install` honours `:force`.
plan 15;

my $tmp = $*TMPDIR.add( 'repo-subclass-' ~ $*PID );
END { run 'rm', '-rf', ~$tmp if $tmp.e }
my $dist-dir = $tmp.add('dist-src');
$dist-dir.add('lib/Sub').mkdir;
$dist-dir.add('META6.json').spurt: '{"name":"Sub::Dist","ver":"1.0","auth":"zef:t","api":"0","provides":{"Sub::Dist":"lib/Sub/Dist.rakumod"}}';
$dist-dir.add('lib/Sub/Dist.rakumod').spurt: 'unit module Sub::Dist; sub sd-hello is export { "hi" }';
my $dist = Distribution::Path.new( $dist-dir );

class MyRepo is CompUnit::Repository::Installation {
  has $.tag = 'plain';
  method short-id { 'mine' }
}

my $repo = MyRepo.new( prefix => $tmp.add('mine').absolute, tag => 'x' );
is $repo.^name, 'MyRepo', 'the subclass instance keeps its class';
is $repo.tag, 'x', 'its own attribute';
is $repo.short-id, 'mine', 'a user method wins over the native row';
is $repo.prefix.absolute, $tmp.add('mine').absolute, 'the native prefix row answers on it';

$repo.install( $dist );
ok $repo.installed.grep({ .meta<name> eq 'Sub::Dist' }).elems == 1, 'install through the subclass instance';
dies-ok { $repo.install( $dist ) }, 'a second install of the same dist dies';
lives-ok { $repo.install( $dist, :force ) }, ':force installs over it';
ok $repo.installed.grep({ .meta<name> eq 'Sub::Dist' }).elems == 1, 'and it is still listed once';
ok $repo.candidates( CompUnit::DependencySpecification.new( short-name => 'Sub::Dist' ) ), 'candidates finds it';

my $parent = CompUnit::RepositoryRegistry.repository-for-name('site');
my $stage = CompUnit::Repository::Staging.new( prefix => $tmp.add('stage').absolute, name => 'site' );
is $stage.short-id, 'staging', 'Staging overrides short-id';
like $stage.path-spec, / '#name(site)#' /, 'and path-spec';
isa-ok $stage, CompUnit::Repository::Installation, 'it is an Installation';
$stage.install( $dist );
ok $stage.installed.grep({ .meta<name> eq 'Sub::Dist' }).elems == 1, 'installs into its own prefix';
lives-ok { $stage.self-destruct }, 'self-destruct';
nok $tmp.add('stage').e, 'removes the staging directory';
