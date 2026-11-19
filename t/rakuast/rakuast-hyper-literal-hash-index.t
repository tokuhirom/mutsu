use v6.e.PREVIEW;
use Test;

plan 8;

# `%h>>.<k>` is MetaPostfix::Hyper over Postcircumfix::LiteralHashIndex, not an
# explicit `.AT-KEY` call (identical under rakudo).
my $one = Q|my %h; %h>>.<k>|.AST.statements[1].expression;
my $hyper = $one.postfix;
isa-ok $hyper, RakuAST::MetaPostfix::Hyper, 'hyper postfix';
isa-ok $hyper.postfix, RakuAST::Postcircumfix::LiteralHashIndex, 'literal hash index';
is $hyper.postfix.index.segments[0].value, 'k', 'one key kept as text';

my $two = Q|my %h; %h>>.<a b>|.AST.statements[1].expression;
is $two.postfix.postfix.index.segments[0].value, 'a b', 'several keys kept as one word quote';

# The explicit method call is unchanged.
my $call = Q|my %h; %h>>.AT-KEY("k")|.AST.statements[1].expression;
isa-ok $call.postfix.postfix, RakuAST::Call::Method, 'explicit AT-KEY stays a call';

# Behaviour of the round-tripped tree.
is-deeply EVAL(Q|my %g = a => 5; (%g, %g)>>.<a>|.AST).List, (5, 5), 'one key evaluates';
is-deeply EVAL(Q|my %g = a => 5, b => 6; (%g, %g)>>.<a b>|.AST).List.map(*.List).List,
    ((5, 6), (5, 6)), 'a slice evaluates';
is-deeply EVAL(Q|my %g = a => 5; (%g, %g)>>.<a>|.AST.DEPARSE).List, (5, 5), 'deparsed text evaluates';
