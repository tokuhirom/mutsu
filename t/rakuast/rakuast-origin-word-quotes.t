use Test;

# Word quotes and literal hash subscripts carry the origins Rakudo gives them
# (ADR-10723 Stage 2): the text of `<a b>` / `<<a b>>` / `«a b»` kept raw, and
# the `<...>` subscript of `%h<a b>` with the word quote between its brackets.
# Ranges measured on rakudo 2026.09.

plan 14;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
my %h = a => 1, b => 2;
say %h<a>;
say %h<a b>;
say %h<<a>>;
say <a  b>;
say <<a b>>;
say «a b»;
say %h<a>:exists;
END
my @s = $src.AST.statements;
sub arg($i) { parts(parts(@s[$i].expression)[1])[0] }

my $one = parts(arg(1))[1];
is range($one), '30..33', 'a literal hash index';
is range(parts($one)[0]), '31..32', 'its word quote';
is range(parts(parts($one)[0])[0]), '31..32', 'and the text in it';

my $two = parts(arg(2))[1];
is range($two), '41..46', 'a subscript of two keys';
is range(parts($two)[0]), '42..45', 'its word quote is the text between the brackets';

my $double = parts(arg(3))[1];
is range($double), '54..59', 'a double-angle subscript';
is range(parts($double)[0]), '56..57', 'its quote is inside the double angles';

my $words = arg(4);
is range($words), '65..71', 'a word quote';
is range(parts($words)[0]), '66..70', 'its text keeps the raw spacing';
is range(parts(arg(5))[0]), '79..82', 'the text of a double-angle word quote';
is range(parts(arg(6))[0]), '91..94', 'the text of a French-quoted word quote';

my $exists = parts(arg(7))[1];
is range($exists), '103..113', 'a subscript with an adverb';
is range(parts($exists)[1]), '106..113', 'the adverb after the closing bracket';

# Text that differs from the raw bracket contents is not placed (Rakudo keeps
# the padding in the range; mutsu holds the key trimmed and leaves it absent).
my @found;
sub find($node) {
    @found.push: $node if $node.^name eq 'RakuAST::Postcircumfix::LiteralHashIndex';
    $node.visit-children(&find);
}
'my %h; %h< a >'.AST.visit-children(&find);
is range(parts(@found[0])[0]), 'none', 'a padded key has no quote range';
