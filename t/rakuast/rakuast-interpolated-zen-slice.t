use Test;

# A zen slice inside an interpolating string keeps its subscript in the tree:
# `"@x[]"` is `ApplyPostfix(Var, Postcircumfix::ArrayIndex)` as in rakudo (#14820).

plan 9;

sub shape($node, $depth = 0) {
    my @lines = ('  ' x $depth) ~ $node.^name;
    $node.visit-children(-> $child { @lines.append(shape($child, $depth + 1)) });
    @lines.flat
}

for '@x[]', '@x.[]' -> $zen {
    my $segment = "my \@x; \"a $zen b\"".AST.statements[1].expression.segments[1];
    is $segment.^name, 'RakuAST::ApplyPostfix', "$zen is an ApplyPostfix in a string";
    is $segment.postfix.^name, 'RakuAST::Postcircumfix::ArrayIndex', "$zen has an ArrayIndex";
}

my $hash = 'my %h; "%h{}"'.AST.statements[1].expression.segments[0];
is $hash.postfix.^name, 'RakuAST::Postcircumfix::HashIndex', '"%h{}" has a HashIndex';
is-deeply shape($hash), shape('my %h; %h{}'.AST.statements[1].expression),
    'same shape as outside a string';

my @x = 1, 2;
my %h = a => 1;
is "a @x[] b", 'a 1 2 b', '@x[] still interpolates the array';
is "@x.[]", '1 2', '@x.[] still interpolates the array';
is "%h{}", "a\t1", '%h{} still interpolates the hash';
