use Test;

# #14916 (accessor survey): the builders and queries of `RakuAST::Doc::Paragraph`
# and `RakuAST::Doc::Markup`, as on rakudo 2026.09: `Paragraph.from-string`
# (answers the text), `leading-whitespace`, the in-place `add-atom`,
# `Markup.add-atom` / `add-meta` (answer what they add), `set-atoms` /
# `set-meta` / `set-opener` / `set-closer` (answer `Nil`) and `Markup.flatten`.

plan 35;

sub markup($letter, @atoms, @meta = ()) {
    RakuAST::Doc::Markup.new(letter => $letter, atoms => @atoms, meta => @meta)
}

# --- Paragraph.from-string / leading-whitespace ---------------------------
is RakuAST::Doc::Paragraph.from-string('hello'), 'hello', 'from-string answers the text';
is RakuAST::Doc::Paragraph.from-string("a\nb"), "a\nb", 'a multi-line one too';
isa-ok RakuAST::Doc::Paragraph.from-string('x'), Str, 'it is a Str';

for ('', 'a', ' a', "  a b", "\ta", "\n  a", " \n a", "a\n  b", '   ', "\t \tx") -> $s {
    is RakuAST::Doc::Paragraph.new($s).leading-whitespace, ($s ~~ /^(\s*)/).Str,
        "leading-whitespace of {$s.raku}";
}
is RakuAST::Doc::Paragraph.new.leading-whitespace, '', 'an empty paragraph has none';
is RakuAST::Doc::Paragraph.new('  a', '  b').leading-whitespace, '  ', 'only the first atom counts';
is RakuAST::Doc::Paragraph.new(markup('B', ['x']), ' a').leading-whitespace, '',
    'a first atom that is not text has none';

# --- Paragraph.add-atom ---------------------------------------------------
my $p = RakuAST::Doc::Paragraph.new('a');
is $p.add-atom('b'), 'b', 'add-atom answers the atom';
is $p.atoms.join('|'), 'a|b', 'and appends it in place';
my $m = markup('I', ['z']);
$p.add-atom($m);
is $p.atoms.elems, 3, 'a nested code can be added';

# --- Markup builders ------------------------------------------------------
my $k = markup('B', ['x']);
is $k.add-atom('y'), 'y', 'Markup.add-atom answers the atom';
is $k.atoms.join('|'), 'x|y', 'and appends it';
ok !$k.set-atoms(('p', 'q')).defined, 'set-atoms answers an undefined value';
is $k.atoms.join('|'), 'p|q', 'and replaces the atoms';
$k.set-atoms('solo');
is $k.atoms.join('|'), 'solo', 'a lone value is a list of one';
is $k.add-meta('m'), 'm', 'add-meta answers the item';
$k.add-meta('n');
is $k.meta.join('|'), 'm|n', 'and appends it';
$k.set-meta(('r',));
is $k.meta.join('|'), 'r', 'set-meta replaces the meta';
ok !$k.set-opener('«').defined, 'set-opener answers an undefined value';
$k.set-closer('»');
is $k.opener ~ $k.closer, '«»', 'the delimiters are replaced';
throws-like { $k.set-opener(5) }, Exception, 'a delimiter must be a Str';

# --- Markup.flatten -------------------------------------------------------
is markup('B', ['x', 'y']).flatten, 'xy', 'flatten joins the atoms';
is markup('B', ['a', markup('X', ['p'], ['q'])]).flatten, 'aX<p|q>', 'a nested code is spelled out';
is markup('B', [markup('I', [markup('C', ['z'])])]).flatten, 'I<C<z>>', 'also when nested twice';
is markup('X', ['a'], ['q']).flatten, 'a|q', 'the meta follows the atoms';
is markup('B', []).flatten, '', 'an empty code is empty';
