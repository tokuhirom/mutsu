use Test;

# Triple-delimited formatting codes (`I<<< q >>>`) in a Pod paragraph are a
# `RakuAST::Doc::Markup` with `opener => "<<<"` (#14880). Their content is one
# raw string: blanks are kept and a nested `<y>` is text, not a code. Shapes
# measured on rakudo 2026.09.

plan 6;

use experimental :rakuast;

sub para(Str $body) {
    ("=begin pod\n$body\n=end pod\n").AST.statements[0].paragraphs[0].raku.subst(/\s+/, ' ', :g)
}
sub markup(Str $letter, Str $content) {
    qq{RakuAST::Doc::Markup.new( letter => "$letter", opener => "<<<", closer => ">>>", atoms => ( "$content", ) )}
}

is para('I<<< q >>>'), 'RakuAST::Doc::Paragraph.new( ' ~ markup('I', ' q ') ~ ', "\n" )',
    'blanks inside the delimiters are kept';
is para('C<<<a b>>> x'), 'RakuAST::Doc::Paragraph.new( ' ~ markup('C', 'a b') ~ ', " x\n" )',
    'text after the code';
is para('B<<< x <y> z >>>'), 'RakuAST::Doc::Paragraph.new( ' ~ markup('B', ' x <y> z ') ~ ', "\n" )',
    'a nested angle bracket is text';
is para('B<<< x >>> and B<<y>>'),
    'RakuAST::Doc::Paragraph.new( ' ~ markup('B', ' x ') ~ ', " and ", RakuAST::Doc::Markup.new( letter => "B", opener => "<<", closer => ">>", atoms => ( "y", ) ), "\n" )',
    'next to a double-delimited code';
is RakuAST::Doc::Markup.new(letter => "I", opener => "<<<", closer => ">>>", atoms => (" q ",)).raku.subst(/\s+/, ' ', :g),
    markup('I', ' q '), 'a hand-built triple code prints the same';
is ("=begin pod\nI<<< q >>>\n=end pod\n").AST.statements[0].paragraphs.elems, 1, 'one paragraph';
