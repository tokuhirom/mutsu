use v6;
use Test;

plan 5;

my $dir = "tmp/io-path-dir-test-filter-$*PID".IO;
my @names = <cfg cfg.5 prefix.cfg example.cfg plain>;
mkdir $dir;
for @names -> $name {
    $dir.add($name).spurt($name);
}

is-deeply $dir.dir(test => /^cfg/).map(*.basename).sort.List,
    <cfg cfg.5>,
    'IO::Path.dir applies anchored regex tests to basenames';
is-deeply $dir.dir(test => /cfg/).map(*.basename).sort.List,
    <cfg cfg.5 example.cfg prefix.cfg>,
    'IO::Path.dir applies unanchored regex tests to basenames';
is-deeply $dir.dir(test => 'cfg').map(*.basename).sort.List,
    ('cfg',),
    'IO::Path.dir applies string tests';
is-deeply $dir.dir(test => -> $name { $name.ends-with('.cfg') }).map(*.basename).sort.List,
    <example.cfg prefix.cfg>,
    'IO::Path.dir applies callable tests';
is-deeply $dir.dir(test => *.starts-with('cfg')).map(*.basename).sort.List,
    <cfg cfg.5>,
    'IO::Path.dir applies WhateverCode tests';

LEAVE {
    for @names -> $name {
        $dir.add($name).unlink if $dir.add($name).e;
    }
    $dir.rmdir if $dir.d;
}
