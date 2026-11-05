use Test;

# #14916 (accessor survey): the in-place builders and config queries of
# `RakuAST::Doc::Block`, as on rakudo 2026.09: `set-type` (Str), `set-level`
# (Int, `0` is none), `set-margin` (Str), `set-paragraphs` (a list, or a lone
# value as a list of one), `add-paragraph`, `set-config` (a Map), `add-config`
# (the last value for a key wins) and the `config-keys` / `config-pairs` /
# `leading-whitespace` queries.

plan 40;

sub para(*@a) { RakuAST::Doc::Paragraph.new(|@a) }
sub blk(*%h) { RakuAST::Doc::Block.new(type => 'para', paragraphs => (para('x'),), |%h) }

# --- set-type / set-margin / set-level -------------------------------------
my $b = blk();
ok !$b.set-type('code').defined, 'set-type answers an undefined value';
is $b.type, 'code', 'and sets the type';
throws-like { $b.set-type(5) }, X::TypeCheck::Binding::Parameter, 'the type must be a Str';

ok !$b.set-margin('  ').defined, 'set-margin answers an undefined value';
is $b.margin, '  ', 'and sets the margin';
throws-like { $b.set-margin(5) }, X::TypeCheck::Binding::Parameter, 'the margin must be a Str';

ok !$b.set-level(2).defined, 'set-level answers an undefined value';
is $b.level, '2', 'and sets the level (read back as a Str)';
$b.set-level(-1);
is $b.level, '-1', 'a negative level is kept';
$b.set-level(0);
is $b.level, '', 'a zero level is no level';
ok !$b.raku.contains('level'), 'and is left out of the constructor form';
throws-like { $b.set-level('2') }, X::TypeCheck::Binding::Parameter, 'the level must be an Int';

my $all = blk();
$all.set-type('code');
$all.set-level(3);
$all.set-margin(' ');
# (not through `blk`, whose default `type` a repeated named argument would have
# to override; see #16762)
my $made = RakuAST::Doc::Block.new(type => 'code', level => 3, margin => ' ', paragraphs => (para('x'),));
is-deeply ($all.type, $all.level, $all.margin), ($made.type, $made.level, $made.margin),
    'the builders give the node what the constructor would';
is $all.raku.lines.map(*.trim.words.head // '').grep(/^ <[a..z]>+ $/).join(','),
    $made.raku.lines.map(*.trim.words.head // '').grep(/^ <[a..z]>+ $/).join(','),
    'and print its fields in the same order';

# --- paragraphs ------------------------------------------------------------
my $p = blk();
ok !$p.set-paragraphs((para('p'), para('q'))).defined, 'set-paragraphs answers an undefined value';
is $p.paragraphs.elems, 2, 'and replaces the paragraphs';
$p.set-paragraphs(para('only'));
is $p.paragraphs.elems, 1, 'a lone value is a list of one';
$p.set-paragraphs(());
is $p.paragraphs.elems, 0, 'an empty list clears them';
my $added = para('z');
ok $p.add-paragraph($added) === $added, 'add-paragraph answers what it added';
is $p.paragraphs.elems, 1, 'and appends it';

# --- config ---------------------------------------------------------------
my $c = blk();
ok !$c.set-config({a => 1}).defined, 'set-config answers an undefined value';
is $c.config.keys.join, 'a', 'and sets the configuration';
is $c.add-config('k', 'v'), 'v', 'add-config answers the value';
is $c.config<k>, 'v', 'and sets the key';
$c.add-config('k', 'w');
is $c.config<k>, 'w', 'the last value for a key wins';
is $c.config.elems, 2, 'and does not duplicate it';
throws-like { $c.add-config(1, 'v') }, X::TypeCheck::Binding::Parameter, 'the key must be a Str';
throws-like { $c.set-config(5) }, X::AdHoc, 'the configuration must be a map or a list of pairs';
ok !$c.set-config(Nil).defined, 'Nil is accepted';
$c.set-config((z => 9,));
is $c.config.keys.join, 'z', 'a list of pairs is accepted';
$c.set-config({a => 1, k => 'w'});
is-deeply $c.config, blk(config => {a => 1, k => 'w'}).config, 'the builders give the node the config the constructor would';

# --- config-keys / config-pairs ---------------------------------------------
my $q = blk(config => {a => 1, b => 2});
is $q.config-keys.sort.join(','), 'a,b', 'config-keys';
is $q.config-pairs.sort.map(*.key).join(','), 'a,b', 'config-pairs';
isa-ok $q.config-keys, List, 'config-keys is a List';
is $q.config-keys.join(','), 'a,b', 'in key order';
is blk().config-keys.elems, 0, 'no config, no keys';

# --- leading-whitespace ------------------------------------------------------
is RakuAST::Doc::Block.new(type => 'para', paragraphs => (para('  hi'),)).leading-whitespace, '  ',
    'the whitespace at the start of the first paragraph';
is blk(margin => '  ').leading-whitespace, '', 'the margin is not counted';
my $none = RakuAST::Doc::Block.new(type => 'para').leading-whitespace;
ok $none === Any, 'a block with no paragraph answers Any';
is RakuAST::Doc::Block.new(type => 'para', paragraphs => ('text',)).leading-whitespace, '', 'a paragraph that is not a node answers an empty string';
