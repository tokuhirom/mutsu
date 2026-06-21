use Test;

# The value of a `given`/`when`/`default` block whose final statement is an
# `if`/`unless` (or a bare block) must be that statement's value — exactly like
# a `do { if ... }` or a sub whose last statement is an `if`. Previously mutsu
# only kept the value when the final statement was a plain expression; a trailing
# `if` was compiled in statement mode, so its value was discarded and the block
# evaluated to Nil.

plan 12;

# `given` whose body is a bare trailing if/else.
sub given-if($t) {
    given $t {
        if $t eq 'x' { 'GIVEN-IF' } else { 'GIVEN-ELSE' }
    }
}
is given-if('x'), 'GIVEN-IF',  'given body ending in if (then) yields the if value';
is given-if('y'), 'GIVEN-ELSE', 'given body ending in if (else) yields the else value';

# `when` block whose final statement is an if/else.
sub when-if($t) {
    given $t {
        when 'a' { if True  { 'WHEN-A-IF' } else { 'WHEN-A-ELSE' } }
        when 'b' { if False { 'WHEN-B-IF' } else { 'WHEN-B-ELSE' } }
        default  { 'DEFAULT' }
    }
}
is when-if('a'), 'WHEN-A-IF',   'when block ending in if (then-taken) yields its value';
is when-if('b'), 'WHEN-B-ELSE', 'when block ending in if (else-taken) yields its value';
is when-if('z'), 'DEFAULT',     'default branch still yields its value';

# `when` block ending in a bare `if` with no else, condition false -> empty
# (the untaken branch yields no value); stringifies to ''.
sub when-if-noelse($t) {
    given $t {
        when 'a' { if False { 'NEVER' } }
        default  { 'DEF' }
    }
}
is ~when-if-noelse('a'), '', 'when block ending in else-less if (false) yields empty';

# `default` block ending in an if.
sub default-if($t) {
    given $t {
        when 'x' { 'X' }
        default  { if $t eq 'q' { 'DEF-Q' } else { 'DEF-OTHER' } }
    }
}
is default-if('q'), 'DEF-Q',     'default block ending in if (then) yields its value';
is default-if('r'), 'DEF-OTHER', 'default block ending in if (else) yields its value';

# `when` block ending in a nested bare block.
sub when-block($t) {
    given $t {
        when 'a' { { 'NESTED-BLOCK' } }
        default  { 'DEF' }
    }
}
is when-block('a'), 'NESTED-BLOCK', 'when block ending in a bare block yields the block value';

# A trailing `if EXPR -> $v { ... }` in value position must bind `$v` to the
# condition value, not read the enclosing topic. (This is what lets the
# Template::Mustache section renderer's `elsif COND -> $_ { ... }` see the right
# `$_` instead of leaking the outer `given` topic.)
sub when-if-bind($t) {
    given $t {
        when 'a' { if 'bound-val' -> $v { "GOT[$v]" } }
        default  { 'DEF' }
    }
}
is when-if-bind('a'), 'GOT[bound-val]',
    'when block ending in `if EXPR -> $v` binds $v to the condition value';

# The binding must not be the enclosing topic even when names collide on `$_`.
sub when-if-bind-underscore($t) {
    given $t {
        when 'sec' { if 'inner' -> $_ { "INNER[$_]" } }
        default    { 'DEF' }
    }
}
is when-if-bind-underscore('sec'), 'INNER[inner]',
    'a trailing `if EXPR -> $_` rebinds $_ rather than reading the given topic';

# Nested if/elsif chain in value position with a bound elsif branch.
sub chain-bound($t) {
    given $t {
        when 'x' {
            if False { 'A' }
            elsif "found-$t" -> $m { "MATCH[$m]" }
            else { 'C' }
        }
        default { 'DEF' }
    }
}
is chain-bound('x'), 'MATCH[found-x]',
    'a bound `elsif EXPR -> $m` branch in value position yields its bound value';
