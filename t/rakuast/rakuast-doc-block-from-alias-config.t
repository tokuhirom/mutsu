use Test;

# #14916 (accessor survey): `RakuAST::Doc::Block.from-alias` and `from-config`,
# as on rakudo 2026.09. Both forward to `new` (so they need a `type`). The
# paragraphs of `from-alias` are the lemma and then the text of the given
# paragraphs, each element ending in a newline (`""` when there are none);
# a missing lemma is the `Any` type object. The only paragraph of
# `from-config` is its key.

plan 21;

sub para(*@a) { RakuAST::Doc::Paragraph.new(|@a) }
sub fa(*%h) { RakuAST::Doc::Block.from-alias(type => 'alias', |%h).paragraphs }

# --- from-alias ---------------------------------------------------------
my $b = RakuAST::Doc::Block.from-alias(type => 'alias', lemma => 'N', paragraphs => ('t',));
isa-ok $b, RakuAST::Doc::Block, 'from-alias builds a Doc::Block';
is $b.type, 'alias', 'with the given type';
is-deeply fa(lemma => 'N', paragraphs => ('t',)).list, ('N', "t\n"), 'the lemma, then the text with a newline';
is-deeply fa(lemma => 'N', paragraphs => ("t\n",)).list, ('N', "t\n\n"), 'a newline already there is not counted';
is-deeply fa(lemma => 'N', paragraphs => ('a', 'b')).list, ('N', "a\nb\n"), 'each element ends in a newline';
is-deeply fa(lemma => 'N', paragraphs => ()).list, ('N', ''), 'no paragraphs: an empty text';
is-deeply fa(lemma => 'N').list, ('N', ''), 'also when they are not given';
is-deeply fa(lemma => 'N', paragraphs => (para('t'),)).list, ('N', "t\n"), 'a paragraph node counts as its text';
is-deeply fa(lemma => 'N', paragraphs => (para('t'), 's')).list, ('N', "t\ns\n"), 'nodes and strings mix';
is-deeply fa(lemma => 'N', paragraphs => ("a\nb",)).list, ('N', "a\nb\n"), 'a multi-line element keeps its lines';
is fa(lemma => 5, paragraphs => ('t',))[0], 5, 'the lemma is kept as given';
ok fa(paragraphs => ('t',))[0] === Any, 'a missing lemma is the Any type object';
is fa(paragraphs => ('t',))[1], "t\n", 'and the text is unchanged';

is RakuAST::Doc::Block.from-alias(type => 'alias', lemma => 'N', paragraphs => (), margin => ' ').margin, ' ',
    'other named arguments go to new';
throws-like { RakuAST::Doc::Block.from-alias(lemma => 'N') }, Exception, 'type is required, as for new';

# --- from-config --------------------------------------------------------
my $c = RakuAST::Doc::Block.from-config(type => 'config', key => 'k');
isa-ok $c, RakuAST::Doc::Block, 'from-config builds a Doc::Block';
is $c.type, 'config', 'with the given type';
is $c.paragraphs.elems, 1, 'one paragraph';
is $c.paragraphs[0], 'k', 'which is the key';
is $c.config.elems, 0, 'and no configuration';
throws-like { RakuAST::Doc::Block.from-config(key => 'k') }, Exception, 'type is required here too';
