use Test;

plan 22;

# Raku clones a block every time its ENCLOSING block runs, and a `state` cell —
# named, or the implicit one behind a bare `$` — belongs to the CLONE. So a
# `state` in a nested block restarts on every execution of the construct that
# contains it, while iterations of one loop execution share it (the loop body is
# the block the loop statement cloned once).
#
# mutsu had this only for real closures (per-clone `state_scope_id`) and loop
# bodies (`reset_state_locals_in_range` at loop-statement entry). An
# inline-compiled `if` branch or bare block had neither, and the interpreter
# carrier path (`classify`/`categorize` and friends) lost the scope entirely.

# --- a nested block restarts, one execution's iterations share ---------------
{
    my @r;
    for ^2 { @r.push: (map { ++$ }, ^3).join(',') }
    is @r.join('|'), "1,2,3|1,2,3", 'a map block inside a for body re-clones';

    my @s;
    for ^3 { @s.push(++$) }
    is @s.join(','), "1,2,3", '...but the for body itself is one clone';

    my @t;
    for ^2 { for ^2 { @t.push(++$) } }
    is @t.join(','), "1,2,1,2", 'a nested for body re-clones per outer iteration';

    my @u;
    for ^2 { my $b = { ++$ }; @u.push($b() ~ $b()) }
    is @u.join(','), "12,12", 'a block literal in a for body re-clones per iteration';

    my @v;
    for ^3 { { @v.push(++$) } }
    is @v.join(','), "1,1,1", 'a bare nested block re-clones per iteration';
}

# --- an `if` branch is a block too ------------------------------------------
{
    sub ifb() { my @r; if 1 { @r.push(++$) }; @r.join(',') }
    is (ifb(), ifb()).join('|'), "1|1", 'an if BLOCK restarts per call';

    sub ifn() { my @r; if 1 { state $n; @r.push(++$n) }; @r.join(',') }
    is (ifn(), ifn()).join('|'), "1|1", '...and so does a named state in one';

    # The branch in VALUE position (last statement of the routine) is the same
    # block; a separate compile path used to miss it.
    sub ifv() { if 1 { state $n; ++$n } }
    is (ifv(), ifv()).join('|'), "1|1", 'an if branch in value position too';

    my $c = 1;
    sub ifd() { my @r; if $c { state $n; @r.push(++$n) }; @r.join(',') }
    is (ifd(), ifd()).join('|'), "1|1", '...and with a runtime condition';

    my @w;
    for ^3 { if $c { state $n; @w.push(++$n) } }
    is @w.join(','), "1,1,1", 'an if branch inside a loop restarts per iteration';

    sub els($x) { my @r; if $x { @r.push('t') } else { state $n; @r.push(++$n) }; @r.join(',') }
    is (els(0), els(0)).join('|'), "1|1", 'an else branch restarts too';
}

# --- the interpreter carrier path keeps the callback's own scope -------------
{
    sub cl() { <a b c>.classify({ ~($ ~= $_) }).keys.sort.join(',') }
    is cl(), "a,ab,abc", 'a classify callback shares one clone per call';
    is cl(), "a,ab,abc", '...and restarts on the next call';

    sub cln() { <a b c>.classify({ state $s; ~($s ~= $_) }).keys.sort.join(',') }
    is cln(), "a,ab,abc", 'a named state in one behaves the same';
    is cln(), "a,ab,abc", '...and restarts on the next call';

    sub cat() { <a b c>.categorize({ state $s; ~($s ~= $_) }).keys.sort.join(',') }
    is cat(), "a,ab,abc", 'categorize calls its mapper once per element';
    is cat(), "a,ab,abc", '...and restarts on the next call';
}

# --- what must NOT restart --------------------------------------------------
{
    sub p() { my $x = ++$; $x }
    is (p(), p(), p()).join(','), "1,2,3",
        'a `$` directly in a routine body keeps counting across calls';

    my $blk = { ++$ };
    is ($blk(), $blk(), $blk()).join(','), "1,2,3",
        'a block held in a variable is one clone';

    is ([ $++ xx 3 ] xx 3).map(*.join(',')).join('|'), "0,1,2|3,4,5|6,7,8",
        'a top-level `$` keeps counting across repeated thunks';

    # A postfix `if`/`unless` introduces NO block, so the statement it gates
    # belongs to the enclosing block — the `if BLOCK` reset must not reach it.
    sub sm() { state $n = 0 if 1; $n++; $n }
    is (sm(), sm(), sm()).join(','), "1,2,3",
        'a state gated by a postfix if keeps once-only init';

    sub smu() { state $n = 0 unless 0; $n++; $n }
    is (smu(), smu()).join(','), "1,2",
        '...and so does one gated by a postfix unless';
}
