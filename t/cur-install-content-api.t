use Test;

plan 5;

# CompUnit::Repository::Installation.install must locate a distribution's sources
# through the distribution's own `.content($name-path)` (the API every
# Distribution implements, S22), NOT by joining a `prefix` attribute onto the
# address. Distribution classes disagree on that attribute: rakudo's
# Distribution::Path has $.prefix, but zef's Zef::Distribution::Local has
# $.path/$.IO and no `prefix` at all. With the join, install silently copied
# nothing and still recorded `provides` entries pointing at files it never wrote.

my $tmp = $*TMPDIR.child("mutsu-cur-install-{$*PID}");
my $src = $tmp.child("dist");
$src.child("lib").mkdir;
$src.child("lib/Greet.rakumod").spurt("unit module Greet;\nsub hi() is export { 'hi' }\n");

# A distribution exposing ONLY the content API — no `prefix` attribute.
class ContentOnlyDist {
    has $.path;
    has $.IO;
    has %.meta;
    method content($name-path --> IO::Handle:D) {
        IO::Handle.new: path => IO::Path.new($name-path, :CWD(self.IO));
    }
}

my %meta =
    name     => 'Greet',
    ver      => '0.1',
    auth     => 'test',
    api      => '',
    provides => { 'Greet' => 'lib/Greet.rakumod' },
;
my $dist = ContentOnlyDist.new(
    path => $src.absolute,
    IO   => $src.absolute.IO,
    meta => %meta,
);

my $repo-dir = $tmp.child("repo");
$repo-dir.mkdir;
my $cur = CompUnit::RepositoryRegistry.repository-for-spec("inst#{$repo-dir.absolute}");
ok $cur.defined, 'got an Installation repository for the temp prefix';

lives-ok { $cur.install($dist) }, 'install of a prefix-less distribution lives';

my $dist-json = $repo-dir.child('dist').dir.grep(*.extension eq 'json').head;
ok $dist-json.defined, 'the dist metadata was written';

# The point of the fix: the recorded source id must name a file that EXISTS.
my %installed = Rakudo::Internals::JSON.from-json($dist-json.slurp);
my $source-id = %installed<provides><Greet><file>;
ok $source-id.defined, 'provides recorded a source id';
ok $repo-dir.child('sources').child($source-id).e,
    'the source file the metadata points at was actually copied';

END { try $tmp.&rmdir-recursive if $tmp.e }
sub rmdir-recursive($p) { for $p.dir { $_.d ?? rmdir-recursive($_) !! $_.unlink }; $p.rmdir }
