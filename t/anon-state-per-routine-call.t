use Test;

plan 19;

# A bare `$` is an anonymous state variable belonging to its enclosing block's
# CLONE. A named routine's own body is cloned once (at registration), so a `$`
# written directly in it persists across calls; the blocks inside that routine
# are re-cloned on every call, so a `$` in one of them restarts. mutsu keyed the
# counter by its compile-time name alone and so kept counting everywhere.
#
# Found in Digest::RIPEMD, whose output stage rotates the five hash words with
# `map { $_[[^5].rotate(++$)] }` — so the second and later `rmd160(...)` calls
# in one process returned a correct-but-rotated digest.
#
# The per-routine-call mechanism this file was written for has since been
# replaced: a bare `$` is now an implicit `state` declaration of its block, so
# the unit of reset is the block CLONE, which is finer than a routine call.
# See `t/state-var-per-block-clone.t` for the general rule; every case here is
# unchanged raku behaviour and still holds.

# --- resets: the `$` is in a block inside a routine ------------------------
{
    sub f() { (map { ++$ }, 1, 2, 3).join(',') }
    is f(), "1,2,3", 'a map block counts from 1';
    is f(), "1,2,3", '...and restarts on the next call';

    sub g() { my @r; for ^3 { @r.push(++$) }; @r.join(',') }
    is g(), "1,2,3", 'a for-loop body counts from 1';
    is g(), "1,2,3", '...and restarts on the next call';

    sub h() { (map { $++ }, 1, 2, 3).join(',') }
    is h(), "0,1,2", 'the postfix form counts from 0';
    is h(), "0,1,2", '...and restarts on the next call';

    sub gt() { (gather { take ++$ for ^3 }).join(',') }
    is gt(), "1,2,3", 'a gather body restarts too';
    is gt(), "1,2,3", '...on the next call';

    sub wh() { my @r; my $i = 0; while $i++ < 3 { @r.push(++$) }; @r.join(',') }
    is wh(), "1,2,3", 'a while-loop body counts from 1';
    is wh(), "1,2,3", '...and restarts on the next call';

    sub ifb() { my @r; if 1 { @r.push(++$) }; @r.join(',') }
    is (ifb(), ifb()).join('|'), "1|1", 'an if BLOCK restarts per call';

    sub cl() { <a b c>.classify({ ~($ ~= $_) }).keys.sort.join(',') }
    is cl(), "a,ab,abc", 'a classify callback shares one clone per call';
    is cl(), "a,ab,abc", '...and restarts on the next call';
}

# --- persists: the `$` is NOT in a nested block inside a routine ------------
{
    sub p() { my $x = ++$; $x }
    is (p(), p(), p()).join(','), "1,2,3",
        'a `$` directly in a routine body keeps counting across calls';

    # A statement modifier introduces no block, so its `$` is the routine's own.
    sub k() { my $r; $r = ++$ for ^3; $r }
    is (k(), k()).join(','), "3,6",
        'a `for` STATEMENT MODIFIER keeps counting across calls';

    my $blk = { ++$ };
    is ($blk(), $blk(), $blk()).join(','), "1,2,3",
        'a block held in a variable is one clone';

    my class C { method m { $++ } }
    my $c = C.new;
    is ($c.m, $c.m, $c.m).join(','), "0,1,2",
        'a `$` directly in a method body keeps counting';

    # ...and a block created inside a routine is one clone for that call.
    sub sv() { my $b = { ++$ }; ($b(), $b()).join(',') }
    is (sv(), sv()).join('|'), "1,2|1,2",
        'a block held in a routine-local variable restarts per call';
}

# --- the mainline has no routine to reset against --------------------------
is ([ $++ xx 3 ] xx 3).map(*.join(',')).join('|'), "0,1,2|3,4,5|6,7,8",
    'a top-level `$` keeps counting across repeated thunks';
