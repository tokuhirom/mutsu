use v6;
use Test;

# ADR-0019 E9-pre found that `lastcall` inside a method wrapper empties the
# wrap chain (as it should), but a following `callsame` died with
# "callsame is not in the dynamic scope of a dispatcher" instead of
# returning Nil (the chain is exhausted, the original method never runs).
# Verified against Rakudo v2026.06.

class C { method m() { say "orig"; "o" } }

my @log;
C.^lookup('m').wrap(-> |c {
    @log.push('wrap');
    lastcall;
    my $r = callsame;
    @log.push("after({$r // 'Nil'})");
    "w";
});

is C.new.m, 'w', 'wrapper return value used, original method never ran';
is @log.join('|'), 'wrap|after(Nil)', 'callsame after lastcall resolves to Nil, in order';

done-testing;
