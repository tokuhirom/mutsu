use experimental :rakuast;
use Test;

# `.++` / `.--` increment or decrement the topic. RakuAST keeps them as a
# `Term::TopicCall` of the `Postfix`, not an `ApplyPostfix` over `$_` (#14880,
# the `.DEPARSE` corpus's S03-operators/autoincrement.t).

plan 10;

sub deparse($src) { $src.AST.statements[*-1].DEPARSE }

is deparse(Q{.++}), '.++', '.++';
is deparse(Q{.--}), '.--', '.--';
is deparse(Q{$_++}), '$_++', '$_++ is still an ApplyPostfix';
is deparse(Q{.++ for @a}), '.++ for @a', 'under a statement modifier';
isa-ok Q{.++}.AST.statements[0].expression, RakuAST::Term::TopicCall, 'the node is a topic call';

$_ = 4;
.++;
is $_, 5, '.++ increments the topic';
.--;
.--;
is $_, 3, '.-- decrements it';
my @a = 1, 2;
.++ for @a;
is-deeply @a, [2, 3], 'through a for modifier';
is EVAL(Q{$_ = 7; .++; $_}.AST), 8, 'EVAL of the .AST runs it';
$_ = 1;
my $r = .++;
is "$r $_", '1 2', 'its value is the value before';
