# rakudo's compiler accumulates worries, sorrows and at most one panic, and only
# collapses to a single exception when it collected exactly one thing. Anything
# else is an `X::Comp::Group`. mutsu raises one typed exception per parse
# failure, so each site that reproduces a rakudo diagnosis with a companion
# complaint has to say so — these are the four roast asks about
# (S16-io/bare-say.t, S02-literals/underscores.t, S04-statements/for.t,
# S04-declarations/my-6e.t).
use Test;

plan 15;

# A bare `say`/`print`/`put`: a worry explaining the bare form, then a panic on
# the missing argument list.
for <say print put> -> $name {
    throws-like $name, X::Comp::Group, "bare $name is a compiler error";
}

# Two or more underscores between digits. A lone `_` separator stays legal, and
# the neighbouring shapes keep their own (different) exception classes.
throws-like { EVAL '1__0' }, X::Comp::Group, 'multiple embedded underscores';
is 1_0, 10, 'a single embedded underscore is still a separator';
throws-like { EVAL '10_' }, X::Syntax::Confused, 'a trailing underscore is Confused, not a group';

# A block eaten by the iterable expression. `for 1..2` really is just missing
# its block and must stay X::Syntax::Missing.
throws-like 'for 1.. { }', X::Comp::Group, 'range endpoint gobbled the block';
throws-like 'for 1... { }', X::Comp::Group, 'sequence endpoint gobbled the block';
throws-like 'for 1, {a=>1}', X::Comp::Group, 'trailing hash-looking block gobbled';
throws-like 'for 1, 2, { say 3 }', X::Comp::Group, 'trailing block gobbled by the comma list';
throws-like 'for 1..2', X::Syntax::Missing, 'a genuinely missing block is not a group';

# One redundant `of` is a lone sorrow; two are a group.
throws-like ｢my Int $a of Str｣, X::Syntax::Variable::ConflictingTypes,
    'one conflicting type is thrown on its own';
throws-like ｢my Int $a of Str is default("z") of Rat｣, X::Comp::Group,
    'two conflicting types are grouped';

# The group keeps its parts, so a caller can read them back.
{
    my $group = ((try EVAL 'say') // $!);
    # rakudo line-wraps the rendered worry, so match on substance, not layout.
    ok $group.worries».message.head.contains('Unsupported use of bare "say"'),
        'the bare-say advice is the group worry';
    is $group.panic.message, 'Argument to "say" seems to be malformed',
        'the missing argument list is the group panic';
}
