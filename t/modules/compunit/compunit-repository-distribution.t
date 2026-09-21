use lib $?FILE.IO.parent(3).add('fixtures/lib-precedence/plain').Str;
use Test;

plan 6;

# Reduction from the Pluggable 0.6 ecosystem suite: a FileSystem repository
# must expose the modules below a `use lib` directory through .distribution.
my $root = $*TMPDIR.add("mutsu-compunit-distribution-{$*PID}-{now.Int}");
my $lib = $root.add('lib');
my $plugins = $lib.add('CaseA').add('Plugins');
$plugins.mkdir(:parents);
$plugins.add('Class1.rakumod').spurt("unit class CaseA::Plugins::Class1;\n");
$plugins.add('Class2.pm6').spurt("unit class CaseA::Plugins::Class2;\n");

my $repo = CompUnit::Repository::FileSystem.new(prefix => $lib.Str);
my @chain = $*REPO.repo-chain;
isa-ok @chain[0], CompUnit::Repository::FileSystem,
    'use lib adds a FileSystem repository to the repository chain';
is @chain[0].prefix.absolute.Str,
    $?FILE.IO.parent(3).add('fixtures/lib-precedence/plain').absolute.Str,
    'use lib repository keeps its absolute prefix';
my @distributions = $repo.distribution;
is @distributions.elems, 1, '.distribution returns one synthetic distribution';
my %provides = @distributions[0].meta<provides>;
ok %provides<CaseA::Plugins::Class1>.defined,
    'distribution metadata includes nested .rakumod modules';
is %provides<CaseA::Plugins::Class1>, 'lib/CaseA/Plugins/Class1.rakumod',
    'nested module metadata has the expected relative path';
ok %provides<CaseA::Plugins::Class2>.defined,
    'distribution metadata includes .pm6 modules';

END {
    try { $plugins.add('Class1.rakumod').unlink };
    try { $plugins.add('Class2.pm6').unlink };
    try { $plugins.rmdir };
    try { $lib.add('CaseA').rmdir };
    try { $lib.rmdir };
    try { $root.rmdir };
}
