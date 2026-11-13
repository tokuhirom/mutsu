use v6;
use Test;

# A module's `END` runs at exit, when `?FILE` is the main script's. A closure
# built in its body (`if COND -> uint $t { ... }`, `-> $t { ... }`, an anonymous
# sub) recorded that as its compunit, so it could not call the module's own
# `my sub` ("Unknown function"). Found with ParaSeq's END summary. Same as raku.

plan 4;

my $p = run $*EXECUTABLE, '-I', 't/lib-end-module-closure', '-e',
    'use EndClosure; add();', :err, :out;
my $err = $p.err.slurp(:close);
is $p.exitcode, 0, 'exits cleanly';
like $err, /'typed-if 1'/, 'typed pointy in END calls the module my sub';
like $err, /'closure 1'/, 'closure in END calls the module my sub';
like $err, /'anon-sub 1'/, 'anonymous sub in END calls the module my sub';
done-testing;
