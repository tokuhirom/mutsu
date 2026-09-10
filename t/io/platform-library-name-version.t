use v6;
use Test;

# A distribution ships the ABI-versioned shared library (`libpq.so.5`) and only
# the `-dev` package installs the bare `libpq.so` symlink, so a binding that
# probes versions must be able to build the versioned name. `NativeLibs`'
# `Searcher.at-runtime('pq', 'PQstatus', 5)` — which DBIish's Pg driver uses —
# goes through `$*VM.platform-library-name($name.IO, :$version)`, which used to
# drop the adverb and hand back the unversioned name for every candidate.

plan 5;

my $plain = $*VM.platform-library-name('pq'.IO).Str;
my $v5 = $*VM.platform-library-name('pq'.IO, :version(Version.new(5))).Str;
my $v516 = $*VM.platform-library-name('pq'.IO, :version(Version.new('5.16'))).Str;
my $undef = $*VM.platform-library-name('pq'.IO, :version(Version)).Str;

# The extension is platform-specific, so assert the shape rather than a literal.
like $plain, /^ 'lib' 'pq' \W/, 'the unversioned name still carries the library name';
isnt $v5, $plain, ':version changes the name it builds';
ok $v5.contains('5'), 'the version appears in the versioned name';
ok $v516.contains('5.16'), 'a multi-part version appears in full';
is $undef, $plain, 'an undefined Version yields the unversioned name';
