use Test;

plan 6;

# The method-redispatch path (`nextsame`/`samewith`/multi-method deferral)
# identifies the chosen candidate by its body fingerprint to skip it in the
# remaining-candidate list. That fingerprint is now memoized by body-Arc pointer
# (method_body_fp_cache); these tests lock in that the memoization preserves
# exact dispatch behavior.

# nextsame walks to the next more-general multi candidate.
my class N {
    multi method m(Int $x) { "int:" ~ callsame() }
    multi method m($x)     { "any:$x" }
}
is N.new.m(5),   'int:any:5', 'callsame from Int candidate reaches Any candidate';
is N.new.m('z'), 'any:z',     'non-Int dispatches straight to Any candidate';

# samewith re-dispatches with new args through the same multi.
my class S {
    multi method m(Int $x) { samewith $x.Str }
    multi method m(Str $x) { "str:$x" }
}
is S.new.m(42), 'str:42', 'samewith redispatches Int -> Str candidate';

# Repeated calls must stay correct (cache hit path, many iterations).
my @out;
my class R {
    multi method m(Int $x) { @out.push('i'); samewith $x.Str }
    multi method m(Str)    { @out.push('s') }
}
R.new.m($_) for ^50;
is @out.grep('i').elems, 50, 'samewith Int leg ran 50x under cache';
is @out.grep('s').elems, 50, 'samewith Str leg ran 50x under cache';

# nextsame in a loop (the defer-next.t shape) keeps producing the same result.
my @r;
my class L {
    multi method m(Int $x where * > 0) { @r.push('>0'); nextsame }
    multi method m(Int $x where * < 10){ @r.push('<10'); nextsame }
    multi method m(Int)                 { @r.push('generic') }
}
L.new.m(5) for ^3;
is @r, ['>0','<10','generic', '>0','<10','generic', '>0','<10','generic'],
    'nextsame chain stable across repeated calls';
