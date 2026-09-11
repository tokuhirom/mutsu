use v6;
use Test;

plan 5;

# A sigil-prefixed alias around a silent subrule still exposes a Match under
# the alias name, and the original subrule action must run for that Match.
# The angle-bracket spelling (<int=.uint>) already covered the action path;
# this pins the equivalent `$<int>=<.uint>` form.
grammar G {
    token uint { \d+ }
    rule e { $<int>=<.uint> }
}

class Actions {
    has @.calls;
    method uint($/) {
        @!calls.push(~$/);
        make $/.Int;
    }
}

my $actions = Actions.new;
my $m = G.subparse('3', :rule<e>, :actions($actions));
ok $m, 'the aliased silent subrule matches';
is $m<int>.ast, 3, 'the subrule action populates the alias AST';
is $m<int>.made, 3, 'the alias exposes the subrule action result';
is-deeply [$m<int>.from, $m<int>.to], [0, 1], 'the alias retains the subrule span';
is-deeply $actions.calls, ['3'], 'the subrule action fires exactly once';
