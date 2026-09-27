use Test;

# Found through the ecosystem distribution Pod::TreeWalker
# (t/basic.rakutest, t/lists.rakutest, t/declarators.rakutest).

plan 14;

=begin pod

=comment Trenchant

=for item1 :numbered
First

=for item2
Second

=begin item1 :numbered
Third
=end item1

=for head2 :id<h>
Heady

=end pod

=comment top level
and more

my @c = $=pod[0].contents;

# Abbreviated `=comment` keeps the text on its own directive line.
isa-ok @c[0], Pod::Block::Comment, 'abbreviated =comment is a comment block';
is-deeply @c[0].contents, ["Trenchant\n"], 'abbreviated =comment keeps its inline text';
is-deeply $=pod[1].contents, ["top level\nand more\n"],
    'top-level abbreviated =comment keeps inline text and continuation';

# `=for itemN` is a Pod::Item of level N, not a named block.
isa-ok @c[1], Pod::Item, '=for item1 is a Pod::Item';
is @c[1].level, 1, '=for item1 has level 1';
is-deeply @c[1].config, { :numbered }, '=for item1 keeps its config';
isa-ok @c[2], Pod::Item, '=for item2 is a Pod::Item';
is @c[2].level, 2, '=for item2 has level 2';

# `=begin itemN :cfg` keeps its config too.
isa-ok @c[3], Pod::Item, '=begin item1 is a Pod::Item';
is-deeply @c[3].config, { :numbered }, '=begin item1 keeps its config';
is-deeply @c[4].config, { :id<h> }, '=for head2 keeps its config';

# Attribute.WHY returns the $=pod declarator block and makes the attribute
# its WHEREFORE (Rakudo's `$!why.set_docee(self)`).
#| before class
class Foo {
    has $.foo; #= a foo!
}
my $attr = Foo.^attributes.first(*.name eq '$!foo');
my $why = $attr.WHY;
my $decl = $=pod.first({ $_ ~~ Pod::Block::Declarator && .contents eq 'a foo!' });
ok $why === $decl, 'Attribute.WHY is the $=pod declarator block';
ok $decl.WHEREFORE === $attr, 'Attribute.WHY sets the block WHEREFORE to the attribute';
is ~$why, 'a foo!', 'Attribute.WHY still stringifies to the comment';
