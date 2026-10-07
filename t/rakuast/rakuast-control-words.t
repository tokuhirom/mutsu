use v6;
use experimental :rakuast;
use Test;

# `proceed`, `succeed`, `take-rw` and `last`/`next`/`redo` in expression
# position are bare calls (`Call::Name::WithoutParentheses`) in rakudo's tree
# (measured on 2026.09), and each EVALs back to the control flow it names.

plan 13;

sub stmt(Str $src, Int $i = 0) { $src.AST.statements[$i].expression }
sub text($node) { $node.raku.lines.map(*.trim).join(' ') }

# --- statements ---------------------------------------------------------------
is text(stmt('proceed')),
    'RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier("proceed") )',
    'proceed is a bare call';
is text(stmt('succeed')),
    'RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier("succeed") )',
    'a bare succeed is a bare call';
is stmt('succeed 5').args.args[0].value, 5, 'succeed with a value keeps its argument';
is stmt('sub f { take-rw 1 }').body.statement-list.statements[0].expression.name.canonicalize,
    'take-rw', 'take-rw is a bare call named take-rw';

is Q|my $r; given 1 { when 1 { $r = 'a'; proceed }; when 1 { $r ~= 'b' } }; $r|.AST.EVAL,
    'ab', 'proceed falls through to the next when';
is Q|my $r = do given 3 { when 3 { succeed 'x'; 'unreached' } }; $r|.AST.EVAL,
    'x', 'succeed ends the when with its value';
is Q|my $c = 1; my @a = gather { take-rw $c }; @a[0]|.AST.EVAL, 1,
    'take-rw EVALs';

# --- expression position --------------------------------------------------------
my $loop = Q|my @r; for 1..5 { $_ == 3 and next; @r.push($_) }; @r.join(',')|;
is $loop.AST.EVAL, '1,2,4,5', '`COND and next` EVALs';
my $last = Q|my @r; for 1..5 { $_ == 3 and last; @r.push($_) }; @r.join(',')|;
is $last.AST.EVAL, '1,2', '`COND and last` EVALs';
my $found = Q|for 1..2 { $_ == 1 and last }|.AST.statements[0];
is $found.^name, 'RakuAST::Statement::For', 'the loop around it converts';
is text(stmt(Q|my $x; $x or next|, 1).right),
    'RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier("next") )',
    'the operand of `or` is a bare next call';
is text(stmt(Q|my $x; $x or last|, 1).right),
    'RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier("last") )',
    'and a bare last call';
is text(stmt(Q|my $x; $x or redo|, 1).right),
    'RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier("redo") )',
    'and a bare redo call';
