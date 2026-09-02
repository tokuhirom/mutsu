use Test;

# `XML` is a bundled battery: `use XML` resolves with no `-I` and no
# `mzef install`. This smoke test exercises both halves the slot's selection
# criterion requires -- parse an XML string into a DOM tree and serialize a
# built tree back to a string -- so a resolution or round-trip regression is
# caught by `make test`, not only by the release-time battery gate.

use XML;

plan 12;

my $src = '<catalog id="c1"><book lang="en"><title>Raku</title></book><book lang="fr">Livre</book></catalog>';

# --- parse ---------------------------------------------------------------
my $doc = from-xml($src);
isa-ok $doc, XML::Document, 'from-xml builds an XML::Document';
is $doc.root.name, 'catalog', 'the root element is named';
is $doc.root<id>, 'c1', 'an attribute reads by key';

my @books = $doc.root.elements(:TAG<book>);
is @books.elems, 2, 'elements(:TAG) finds both children';
is @books[0]<lang>, 'en', 'a child attribute reads';
is @books[0].elements(:TAG<title>)[0].contents, 'Raku', 'a grandchild text node reads';
is @books[1].contents, 'Livre', 'a direct text child reads';

# --- round-trip ----------------------------------------------------------
is ~$doc, '<?xml version="1.0"?>' ~ $src, 'the parsed document serializes back';

# --- build ---------------------------------------------------------------
my $made = make-xml('rss', :version<2.0>, \('channel', \('title', 'mutsu')));
is ~$made, '<rss version="2.0"><channel><title>mutsu</title></channel></rss>',
    'make-xml builds a nested tree';

my $el = XML::Element.craft('item', :id<7>, 'text');
is ~$el, '<item id="7">text</item>', 'craft builds one element';

# --- mutate --------------------------------------------------------------
$doc.root.append($el);
is $doc.root.elements(:TAG<item>).elems, 1, 'an appended element is found';
$doc.root.set('id', 'c2');
is $doc.root<id>, 'c2', 'set() updates an attribute';
