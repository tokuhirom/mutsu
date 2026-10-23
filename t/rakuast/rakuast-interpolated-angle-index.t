use Test;

# `"%h<a>"` inside a string is rakudo's `Postcircumfix::LiteralHashIndex`
# (#14820), exactly like `%h<a>` written outside a string.

plan 6;

sub shape($node, $depth = 0) {
    my @lines = ('  ' x $depth) ~ $node.^name;
    $node.visit-children(-> $child { @lines.append(shape($child, $depth + 1)) });
    @lines.flat
}

my $inside  = 'my %h; "%h<a>"'.AST.statements[1].expression;
my $outside = 'my %h; %h<a>'.AST.statements[1].expression;

is $inside.^name, 'RakuAST::QuotedString', 'interpolating string is a QuotedString';
my $postfix = $inside.segments[0];
is $postfix.^name, 'RakuAST::ApplyPostfix', 'segment is an ApplyPostfix';
is $postfix.postfix.^name, 'RakuAST::Postcircumfix::LiteralHashIndex',
    'angle subscript in a string is a LiteralHashIndex';
is-deeply shape($postfix), shape($outside), 'same shape as outside a string';

my %h = a => 1, b => 2;
is "v: %h<a> %h<a b>|", 'v: 1 1 2|', 'interpolation still evaluates';
is EVAL('my %h = a => 5; "%h<a>"'.AST), '5', 'EVAL of the converted tree evaluates';
