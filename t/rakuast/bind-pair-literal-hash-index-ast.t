use Test;
use experimental :rakuast;

plan 7;

# Binding a pair to a word-quoted subscript keeps the subscript as written
# (`Postcircumfix::LiteralHashIndex`), as rakudo does (#18471).

sub postfix-of(Str $code) { $code.AST.statements[1].expression.left.postfix }

is postfix-of(Q[my %h; %h<p> := 1 => 2]).^name, 'RakuAST::Postcircumfix::LiteralHashIndex',
    '%h<p> := PAIR';
is postfix-of(Q[my %h; %h«p» := 1 => 2]).^name, 'RakuAST::Postcircumfix::LiteralHashIndex',
    '%h«p» := PAIR';
is postfix-of(Q[my %h; %h{"p"} := 1 => 2]).^name, 'RakuAST::Postcircumfix::HashIndex',
    '%h{"p"} := PAIR';
is postfix-of(Q[my %h; %h<q> := 1]).^name, 'RakuAST::Postcircumfix::LiteralHashIndex',
    '%h<q> := VALUE';

is Q[my %h; %h<p> := 1 => 2].AST.DEPARSE.lines[1], '%h<p> := 1 => 2', 'DEPARSE';
is Q[my %h; %h<p> := 1 => 2; %h<p>.key].AST.EVAL, 1, 'EVAL binds the pair';

my %h;
%h<p> := 1 => 2;
is-deeply %h<p>, (1 => 2), 'the pair is the bound value';
