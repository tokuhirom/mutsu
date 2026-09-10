use Test;

# Rakudo declares ACCEPTS (and the file-test adverb candidates) on IO::Path, so
# every SPEC-variant subclass inherits them. mutsu's smartmatch arms matched the
# exact class name `IO::Path` only, which made every comparison between two
# `IO::Path::Unix` objects fall through to the generic instance comparison and
# answer False -- Test::Util's `is-path` (`cmp-ok $got.resolve, '~~',
# $exp.resolve`) never passed.

plan 10;

my $a = IO::Path::Unix.new('/foo/').add('bar');
my $b = IO::Path::Unix.new('/foo/bar');

ok $a ~~ $b, 'IO::Path::Unix ACCEPTS an equal IO::Path::Unix';
ok $a.resolve ~~ $b.resolve, '... and so do their resolved forms';
nok $a ~~ IO::Path::Unix.new('/foo/baz'), 'a different path does not match';

# The plain class keeps working, and the two mix.
ok IO::Path.new('/foo/bar') ~~ IO::Path.new('/foo/./bar'),
    'IO::Path ACCEPTS an equal IO::Path';
ok IO::Path.new('/foo/bar') ~~ $b, 'IO::Path ACCEPTS an equal subclass instance';
ok $a ~~ IO::Path.new('/foo/bar'), 'a subclass instance ACCEPTS an equal IO::Path';

# Cool ~~ IO::Path::Unix stringifies the LHS and compares absolute paths.
ok '/foo/bar' ~~ $b, 'Str ~~ IO::Path::Unix compares absolute paths';

# IO::Path::Unix ~~ Str stringifies the path.
ok $b ~~ '/foo/bar', 'IO::Path::Unix ~~ Str compares the path string';

# The file-test adverbs are inherited too.
my $self = IO::Path::Unix.new($*PROGRAM.absolute);
ok $self ~~ :e, 'IO::Path::Unix ~~ :e sees an existing file';
nok IO::Path::Unix.new('/no/such/file/here') ~~ :e,
    'IO::Path::Unix ~~ :e is False for a missing file';
