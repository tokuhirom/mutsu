use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 10;

# `supply { ... }` is wrapped around its emitter where the parser built it: the
# body is lowered in a lexical scope of its own and `emit` / `done` bind to the
# block's own emitter. Same behaviour in the program, an EVAL string and an
# EVAL tree.

my $s = supply { emit 1; emit 2; done };
is $s.list.join(','), '1,2', 'emit and done';

my $outer = supply {
    my $inner = supply { emit 'a'; emit 'b' };
    whenever $inner { emit .uc }
};
is $outer.head(2).list.join(','), 'A,B', 'a nested supply keeps its own emitter';

my $declared = supply {
    my $n = 3;
    sub twice { $n * 2 }
    emit twice();
};
is $declared.list.join(','), '6', 'a routine declared in the body';

my $closing = '';
my $c = supply { CLOSE { $closing ~= 'closed' }; emit 1; done };
$c.tap(-> $v { });
is $closing, 'closed', 'a CLOSE phaser runs';

my $got = '';
react {
    whenever supply { emit 1; emit 2; done } { $got ~= $_ }
}
is $got, '12', 'react over a supply';

is EVAL(Q[supply { emit 5; emit 6; done }.list.join(',')]), '5,6', 'EVAL string';
is EVAL(Q[supply { emit 7; done }.list.join(',')].AST), '7', 'EVAL tree';
is EVAL(Q[my $s = supply { my $x = 4; emit $x * 2; done }; $s.list.join(',')].AST), '8', 'EVAL tree: a lexical in the body';

throws-like { EVAL Q[supply { $^a }] }, X::Placeholder::Block, 'a placeholder in the body is rejected';
lives-ok { EVAL Q[my $m = supply { done }] }, 'an empty supply';
