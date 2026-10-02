use v6;
use lib 't/lib';
use Test;

# `&name` inside a module's routine names what that module imported, not a
# same-named `my &name` of whoever called it. Template::HAML's EVAL'd template
# code binds `my &tab-up = -> |c { $ctx.tab-up(|c) }`, and the context's
# `method tab-up(|c) { &tab-up(|c) }` (whose module imports `sub tab-up`) read
# that caller binding and called itself until the stack ran out (#10638).

use AmpScope::Ctx;

plan 7;

my $ctx = AmpScope::Ctx.new;

is $ctx.tab-up, 'sub:1', 'no caller binding: the imported sub';

{
    my &tab-up = -> |c { $ctx.tab-up(|c) };
    is tab-up(), 'sub:1', '&tab-up() in the method ignores the caller binding';
    is tab-up(3), 'sub:3', '... with arguments';
    is $ctx.tab-up-ref, 'tab-up', '&tab-up as a value ignores it too';
}

{
    my $code = EVAL q[my &tab-up = -> |c { $ctx.tab-up(|c) }; -> { tab-up(2) }];
    is $code(), 'sub:2', 'from EVAL-compiled caller code';
}

{
    my &tab-up = sub lexical(|c) { 'lexical' };
    is &tab-up(), 'lexical', 'the caller binding still answers in its own scope';
    is (-> { &tab-up() })(), 'lexical', 'and in a closure that captured it';
}
