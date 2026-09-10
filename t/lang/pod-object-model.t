use Test;

# The `$=pod` object model: `$=pod` is an Array of real `Pod::*` block
# objects, each carrying the attributes rakudo declares (and rendering them
# from `.raku` in rakudo's order), and `.WHY` is a `Pod::Block::Declarator`
# that stringifies to its documentation text.
#
# Everything asserted here was established against `raku` v2026.06, so this
# file must pass under both `raku` and `mutsu`.

plan 46;

=begin pod

=head2 A heading

Text with C<code> and L<display|http://example.com> here.

=item An item

=begin code
line one
line two
=end code

=begin comment
hidden text
=end comment

=defn Term
Definition body.

=end pod

my $doc = $=pod[0];

# --- $=pod is an Array of Pod blocks, not a nested one-element list -------

ok $=pod ~~ Positional, '$=pod is Positional';
ok $=pod.all ~~ Pod::Block, 'every $=pod entry is a Pod::Block object';
isa-ok $doc, Pod::Block::Named, '$=pod[0] is a Pod::Block::Named';
is $doc.name, 'pod', 'the named block is the `pod` block';

# `for $=pod` iterates the blocks themselves: `$=pod` is bound to the
# document, it is not a Scalar item container.
my @seen;
for $=pod -> $block {
    @seen.push: $block.^name;
}
is @seen.elems, $=pod.elems, 'for $=pod iterates every block';
is @seen[0], 'Pod::Block::Named',
   'for $=pod yields the block itself, not a wrapper list';

my @copy = $=pod;
isa-ok @copy[0], Pod::Block::Named, '@a = $=pod copies the blocks';

ok $doc.contents ~~ Positional, '.contents is Positional';
is $doc.contents.elems, 6, 'the pod block has six children';

# --- Pod::Heading ---------------------------------------------------------

my $heading = $doc.contents[0];
isa-ok $heading, Pod::Heading, '=head2 makes a Pod::Heading';
isa-ok $heading, Pod::Block, 'Pod::Heading is a Pod::Block';
isa-ok $heading.level, Int, '.level is an Int, not a Str';
is $heading.level, 2, '=head2 has level 2';
is $heading.config, {}, 'an unconfigured heading has an empty config';
is $heading.raku,
   'Pod::Heading.new(level => 2, config => {}, contents => [Pod::Block::Para.new(config => {}, contents => ["A heading"])])',
   'Pod::Heading.raku round-trips its attributes';

# --- Pod::Block::Para and Pod::FormattingCode -----------------------------

my $para = $doc.contents[1];
isa-ok $para, Pod::Block::Para, 'a plain paragraph is a Pod::Block::Para';
is $para.contents.elems, 5, 'the paragraph splits around its formatting codes';
is $para.contents[0], 'Text with ', 'leading plain text';

my $code-fc = $para.contents[1];
isa-ok $code-fc, Pod::FormattingCode, 'C<> makes a Pod::FormattingCode';
isa-ok $code-fc, Pod::Block, 'Pod::FormattingCode is a Pod::Block';
is $code-fc.type, 'C', '.type is the code letter';
ok $code-fc.meta ~~ Positional, '.meta is always Positional';
is $code-fc.meta.elems, 0, 'C<> carries no meta';
is $code-fc.raku,
   'Pod::FormattingCode.new(type => "C", meta => [], config => {}, contents => ["code"])',
   'Pod::FormattingCode.raku round-trips its attributes';

my $link-fc = $para.contents[3];
is $link-fc.type, 'L', 'L<> makes an L formatting code';
is $link-fc.meta, ['http://example.com'], 'L<display|target> puts the target in .meta';
is $link-fc.contents, ['display'], 'L<display|target> keeps the display text in .contents';

# --- Pod::Item ------------------------------------------------------------

my $item = $doc.contents[2];
isa-ok $item, Pod::Item, '=item makes a Pod::Item';
isa-ok $item.level, Int, 'Pod::Item.level is an Int';
is $item.level, 1, 'a bare =item is level 1';

# --- Pod::Block::Code -----------------------------------------------------

my $code = $doc.contents[3];
isa-ok $code, Pod::Block::Code, '=begin code makes a Pod::Block::Code';
isa-ok $code, Pod::Block, 'Pod::Block::Code is a Pod::Block';
is $code.contents, ['line one', "\n", 'line two', "\n"],
   'an explicit code block keeps one element per line plus its newline';

# --- Pod::Block::Comment --------------------------------------------------

my $comment = $doc.contents[4];
isa-ok $comment, Pod::Block::Comment, '=begin comment makes a Pod::Block::Comment';
isa-ok $comment, Pod::Block, 'Pod::Block::Comment is a Pod::Block';

# --- Pod::Defn ------------------------------------------------------------

my $defn = $doc.contents[5];
isa-ok $defn, Pod::Defn, '=defn makes a Pod::Defn';
isa-ok $defn, Pod::Block, 'Pod::Defn is a Pod::Block';
is $defn.term, 'Term', '.term is the definition term';

# --- .WHY: a Pod::Block::Declarator that stringifies to its text ----------

#| Leading documentation
sub documented($x) {
    $x;
}
#= (trailing documentation)

my $why = &documented.WHY;
isa-ok $why, Pod::Block::Declarator, '.WHY is a Pod::Block::Declarator';
is $why.Str, "Leading documentation\n(trailing documentation)",
   '.WHY.Str joins the leading and trailing text with a newline';
is $why.gist, "Leading documentation\n(trailing documentation)",
   '.WHY.gist renders the documentation, not the type name';
is ~$why, "Leading documentation\n(trailing documentation)",
   '.WHY stringifies in interpolation too';
is $why.leading, 'Leading documentation', '.leading is the #| text';
is $why.trailing, '(trailing documentation)',
   '.trailing keeps the parentheses of a spaced #= comment';
is $why.WHEREFORE.^name, 'Sub', '.WHEREFORE is the documented routine';

#|(bracketed documentation)
sub bracketed() { }
is &bracketed.WHY.Str, 'bracketed documentation',
   'a bracket immediately after #| opens the block form';

# vim: expandtab shiftwidth=4
