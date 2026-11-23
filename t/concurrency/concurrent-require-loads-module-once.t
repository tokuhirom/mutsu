use Test;

# #17216: a module loads once even when several threads `require` it at the
# same moment. The loaded-module table is shared by every thread, but checking
# it and loading were two steps, so threads that all checked before the first
# load finished each ran the module's body again.

plan 4;

constant DIR = $*TMPDIR.add("mutsu-require-once-{$*PID}").Str;
use lib DIR;
my $dir = DIR.IO;
mkdir $dir;
my $log = $dir.add('loads.log');
$dir.add('LoadedOnce.rakumod').spurt: qq:to/MOD/;
    unit module LoadedOnce;
    '{$log}'.IO.spurt("load\\n", :append);
    sleep 0.2;
    our sub hello is export \{ 'hello' }
    MOD
$dir.add('Awaits.rakumod').spurt: q:to/MOD/;
    unit module Awaits;
    our $got = await start { require LoadedOnce; 'inner' };
    MOD

my @results = await (^4).map: { start { require ::('LoadedOnce'); ::('LoadedOnce')::('&hello')() } };
is @results, <hello hello hello hello>, 'every thread sees the loaded module';
is $log.lines.elems, 1, 'its body ran once';

# A module whose mainline awaits a thread that loads another module still
# finishes: the claim is per module, not one process-wide lock.
require ::('Awaits');
is ::('Awaits')::('$got'), 'inner', 'a load can await a thread that loads another module';
is $log.lines.elems, 1, 'the module the other thread asked for was not loaded again';

END { try { .unlink for $dir.dir; $dir.rmdir } }
