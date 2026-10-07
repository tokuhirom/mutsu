use Test;
use lib 't/lib';
use EnumLeakGrammar;
use EnumLeakGrammar::Actions;

# Distilled from PDF::Grammar::COS::Actions (ecosystem dist PDF::Grammar): its
# `array[uint64]` resolved to the `array` member of the grammar's own enum.
plan 3;

is call-back({ array[uint64].new(3).raku }), 'array[uint64].new(3)',
    'a closure handed to a module routine sees the core array type';

class Act { method TOP($/) { make array[int].new(4) } }
is EnumLeakGrammar.parse('x', :actions(Act.new)).made.raku, 'array[int].new(4)',
    'an action class of the main script sees the core array type';

is EnumLeakGrammar.parse('x', :actions(EnumLeakGrammar::Actions.new)).made.raku,
    'array[uint64].new(1, 2)',
    "an action class in another file does not see the grammar body's enum";
