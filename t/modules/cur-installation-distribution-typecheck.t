use v6;
use Test;

plan 4;

# Run against a temporary repository, never the real site repo.
my $dir = $*TMPDIR.add("mutsu-cur-inst-typecheck-{$*PID}-{(^1000000).pick}");
$dir.mkdir;
LEAVE $dir.&{ run 'rm', '-rf', ~$_ };

my $repo = CompUnit::Repository::Installation.new(:prefix($dir.Str));

throws-like { $repo.uninstall(Any) }, X::TypeCheck::Binding::Parameter,
    'uninstall(Any) fails the Distribution type check';
throws-like { $repo.install(Any) }, X::TypeCheck::Binding::Parameter,
    'install(Any) fails the Distribution type check';
throws-like { $repo.uninstall("foo") }, X::TypeCheck::Binding::Parameter,
    'uninstall(Str) fails the Distribution type check';

my $dist = Distribution::Hash.new(
    { name => 'Foo::Bar', ver => '0.1', auth => 'test', api => '0', provides => {} },
    :prefix($dir.add('src')),
);
lives-ok { $repo.uninstall($dist) }, 'a real Distribution passes the type check';
