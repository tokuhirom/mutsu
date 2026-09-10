use v6;
use Test;

plan 12;

# A nested named-alias chain `:mil(:milli(:$millis))` makes every level
# (`mil`, `milli`, `millis`) a valid caller-side key, but inside the body only
# the innermost `$millis` is a variable. The alias names (`mil`, `milli`) and
# the param's own name must NOT be bound as body variables, else they shadow a
# same-named outer constant/lexical (regression: TimeUnit's `nanos-from` read
# the alias `milli`/`min` instead of the module constant).

{
    my constant milli = 1000000;
    sub g(:mil(:milli(:$millis)) = 0) { "millis=$millis milli=" ~ milli }
    is g(mil => 2),    'millis=2 milli=1000000', 'outer alias key + constant not shadowed';
    is g(milli => 3),  'millis=3 milli=1000000', 'middle alias key + constant not shadowed';
    is g(millis => 4), 'millis=4 milli=1000000', 'innermost key + constant not shadowed';
}

# The innermost leaf variable is the only body variable; every alias name binds
# it at the call site.
{
    sub h(:d(:day(:$days)) = 0) { $days }
    is h(d => 7),    7, 'leaf reachable via outer alias';
    is h(day => 8),  8, 'leaf reachable via middle alias';
    is h(days => 9), 9, 'leaf reachable via innermost name';
}

# The param's own name is a caller key only, not a body variable: `min` in the
# body resolves to the outer constant, not the argument.
{
    my constant min = 60;
    sub to-seconds(:min(:$minutes) = 0) { $minutes * min }
    is to-seconds(min => 30), 1800, 'top param name resolves to outer constant, not arg';
}

# A multi sub with rename + where-constraint + default (the TimeUnit shape).
{
    my constant sec = 1000;
    my constant mn  = sec * 60;
    multi sub nanos-from(
        :min(:minute(:$minutes)) where { $_ >= 0 } = 0,
        |c
    ) { $minutes * mn }
    multi sub nanos-from(|c) { die "fallback" }
    is nanos-from(min => 2),    120000, 'multi rename+where: outer alias';
    is nanos-from(minute => 3), 180000, 'multi rename+where: middle alias';
}

# A rename to an anonymous `$` with a `where` constraint: the constraint must be
# checked against the passed value even though no named body variable exists
# (zef's `multi sub MAIN(Bool :version($) where .so)`).
{
    sub want-true(Bool :flag($) where .so) { "matched" }
    is want-true(flag => True), 'matched', 'where on anonymous rename checks passed value';
    dies-ok { want-true(flag => False) }, 'where on anonymous rename rejects failing value';
}

# `:color(:$colour)` single-level rename: caller uses either key, body uses the
# leaf; the outer name `color` is not a body variable.
{
    sub paint(:color(:$colour) = 'none') { $colour }
    is paint(color => 'red'), 'red', 'single-level rename by outer key';
}
