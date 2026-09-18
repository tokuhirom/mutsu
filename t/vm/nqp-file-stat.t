use v6;
use nqp;
use Test;

# MoarVM::Bytecode reaches these predicates and handlers through the paths
# dependency.
plan 18;

is nqp::stat('src/main.rs', nqp::const::STAT_EXISTS), 1,
    'stat reports an existing path';
is nqp::stat('src/main.rs', nqp::const::STAT_FILESIZE) > 0, True,
    'stat reports a regular file size';
is nqp::stat('src/main.rs', nqp::const::STAT_ISREG), 1,
    'stat recognizes a regular file';
is nqp::stat('src/main.rs', nqp::const::STAT_ISDIR), 0,
    'stat does not classify a regular file as a directory';
is nqp::stat('tmp', nqp::const::STAT_EXISTS), 1,
    'stat reports an existing directory';
is nqp::stat('tmp', nqp::const::STAT_ISDIR), 1,
    'stat recognizes a directory';
is nqp::stat('tmp', nqp::const::STAT_ISREG), 0,
    'stat does not classify a directory as a regular file';
is nqp::stat('tmp/mutsu-stat-does-not-exist', nqp::const::STAT_EXISTS), 0,
    'stat reports a missing path as absent';
dies-ok { nqp::stat('tmp/mutsu-stat-does-not-exist', nqp::const::STAT_ISREG) },
    'stat dies when inspecting a missing path';

ok nqp::filereadable('src/main.rs'),
    'filereadable recognizes a readable file';
is nqp::fileislink('src/main.rs'), 0,
    'fileislink rejects a regular file';

my $dir := nqp::opendir('src');
ok !nqp::isnull($dir),
    'opendir returns a directory handle';
my str $entry = nqp::nextfiledir($dir);
ok $entry ne '',
    'nextfiledir returns a directory entry';
nqp::closedir($dir);
is nqp::nextfiledir($dir), '',
    'closedir exhausts the directory handle';

is nqp::handle(42, 'CATCH', 7), 42,
    'handle returns the protected expression value';
is nqp::handle(nqp::opendir('tmp/mutsu-stat-no-such-directory'), 'CATCH', 42), 42,
    'handle returns the CATCH handler value';

my $matcher = * eq 'main.rs';
is $matcher.ACCEPTS('main.rs'), True,
    'WhateverCode ACCEPTS evaluates a matching predicate';
is $matcher.ACCEPTS('other.rs'), False,
    'WhateverCode ACCEPTS rejects a non-matching predicate';
