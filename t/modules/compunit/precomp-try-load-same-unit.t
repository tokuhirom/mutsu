use v6;
use Test;
use nqp;

# try-load of a source file that declares a class, twice: a precompilation
# repository compiles a unit once, so the second load hands back the same
# unit instead of redeclaring the class (Pod::To::HTML `render` on two
# Pod::Load-ed files with the same content died "Redeclaration of symbol").
plan 5;

my $dir = $*TMPDIR.add("mutsu-pc-same-unit-{$*PID}-{(^1000000).pick}");
$dir.mkdir;
LEAVE { .unlink for $dir.add("unit.raku"), $dir.add("other.raku") }
my $f = $dir.add("unit.raku").Str;
spurt $f, "=begin pod\nhello\n=end pod\n\nclass PcSameUnitMagician \{ method m \{ 1 \} \}\n";

my $repo = CompUnit::PrecompilationRepository::Default.new(
    :store(CompUnit::PrecompilationStore::File.new(:prefix($dir.add("cache")))));

sub load-it(Str $path) {
    $repo.try-load(CompUnit::PrecompilationDependency::File.new(
        :src($path),
        :id(CompUnit::PrecompilationId.new-from-string($path)),
        :spec(CompUnit::DependencySpecification.new(:short-name($path)))))
}

my $first = load-it($f);
ok $first.defined, 'first try-load answers a handle';
my $second;
lives-ok { $second = load-it($f) }, 'a second try-load of the same file does not redeclare its class';
ok $second.defined, 'and answers a handle';
is nqp::atkey($second.unit, '$=pod').elems, nqp::atkey($first.unit, '$=pod').elems,
    'both handles expose the same pod';

my $g = $dir.add("other.raku").Str;
spurt $g, "class PcSameUnitOther \{ \}\n";
ok load-it($g).defined, 'a different source still compiles as its own unit';
