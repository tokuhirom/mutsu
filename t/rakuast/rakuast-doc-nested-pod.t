use Test;

# Pod written at column 0 inside the block of a class, sub or `if` is a
# `RakuAST::Doc::Block` statement of that block's `StatementList` (#14880), and
# the list's range is the union of its items, as rakudo 2026.09 shapes them.
# An indented block (rakudo gives it a `margin`) and a `=head1` line that is
# only heredoc text stay out of the tree.

plan 12;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub body-list(Str $src) {
    my $e = $src.AST.statements[0];
    $e = $e.expression if $e ~~ RakuAST::Statement::Expression;
    ($e ~~ RakuAST::Class ?? $e.body.body !! $e.then.body).statement-list
}
sub items($l) { $l.statements.map({ .^name.subst('RakuAST::', '') ~ "@" ~ rng($_) }).join(", ") }

my $first = body-list("class A1 \{\n=begin pod\nin\n=end pod\n  method m \{ 1 }\n}\n");
is items($first), 'Doc::Block@11-33, Statement::Expression@36-50', 'a block before a method';
is rng($first), '11-50', 'the list spans both';
is $first.statements[0].paragraphs[0], "in\n", 'the paragraph';

my $last = body-list("class A2 \{\n  method m \{ 1 }\n=begin pod\nin\n=end pod\n}\n");
is items($last), 'Statement::Expression@13-27, Doc::Block@28-50', 'a block after a method';
is rng($last), '13-50', 'the list spans both';

my $only = body-list("class A3 \{\n=begin pod\nin\n=end pod\n}\n");
is items($only), 'Doc::Block@11-33', 'a block alone in a class body';
is rng($only), '11-33', 'the list is the block';

my $if = body-list("if True \{\n=head1 T\n\nsay 1;\n}\n");
is items($if), 'Doc::Block@10-18, Statement::Expression@20-25', 'an abbreviated block in an if body';
is rng($if), '10-26', 'the list includes the statement terminator';

my $between = body-list("if True \{\n  say 1;\n=head1 T\n\n  say 2;\n}\n");
is items($between), 'Statement::Expression@12-17, Doc::Block@19-27, Statement::Expression@31-36',
    'a block between two statements';
is rng($between), '12-37', 'the list spans all three';

my $heredoc = Q:to/END/.AST.statements[0].expression.body.statement-list;
sub f {
    my $t = q:to/E/;
=head1 X
E
    1
}
END
is $heredoc.statements.elems, 2, 'a heredoc line that looks like Pod is text';
