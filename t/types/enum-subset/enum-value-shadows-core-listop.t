use v6;
use Test;

# An enum value is a lexical symbol like any other, so it shadows a core routine
# of the same name:
#
#     my enum LogLevel <nothing error warn info now debug all>;
#     ...
#     ) if  all   ≤ $!verbose;
#
# (Pakku::Log, with nine more Pakku compunits behind it.) mutsu's parser treated a
# core listop head as one unconditionally, so `all ≤ $!verbose` became a call to
# the junction builtin with `≤ $!verbose` demanded as its argument, and every
# compunit that loaded Pakku::Log failed to parse. A declared enum value is a
# complete nullary term instead.

plan 12;

# The junction builtins, shadowed.
{
    my enum L <nothing debug all>;
    is (all ≤ debug), False, 'a shadowing `all` is a term, not the junction builtin';
    is all.value, 2, 'and it is the enum value';
    is all.key, 'all', 'with the enum key';
}
{
    my enum J <any one none>;
    is any.value, 0, 'a shadowing `any`';
    is one.value, 1, 'a shadowing `one`';
    is none.value, 2, 'a shadowing `none`';
    is (any < none), True, 'they compare as enum values';
}

# A listop that is not a junction: `sort`.
{
    my enum Dir <asc sort desc>;
    is sort.value, 1, 'a shadowing `sort` is the enum value';
    is (sort > asc), True, 'and it compares as one';
}

# The unshadowed builtins still take paren-less arguments.
{
    my @a = 1, 2, 3;
    is (so all(@a) > 0), True, 'the `all` junction still works with parens';
    is (so all @a > 0), True, 'and paren-less';
    is-deeply (sort 3, 1, 2), (1, 2, 3), 'paren-less `sort` still sorts';
}
