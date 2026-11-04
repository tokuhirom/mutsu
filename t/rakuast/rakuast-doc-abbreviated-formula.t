use Test;

# An abbreviated `=formula` holds one line only (#14880): the rest of its own
# line, else the next non-blank line, with each run of blanks one space. The
# lines after it are code, not Pod (the parser used to skip them too).
# Shapes measured on rakudo 2026.09.

plan 13;

use experimental :rakuast;

sub statements(Str $src) { $src.AST.statements.map(*.raku.subst(/\s+/, ' ', :g)).join(" | ") }
sub formula(Str $text) {
    'RakuAST::Doc::Block.new( type => "formula", abbreviated => True, paragraphs => ( "' ~ $text ~ '", ) )'
}
sub span(Str $src) { my $o = $src.AST.statements[0].origin; "{$o.from}-{$o.to}" }

is statements("=formula E = mc^2\n"), formula('E = mc^2'), 'text on the directive line';
is statements("=formula\nE = mc^2\n"), formula('E = mc^2'), 'text on the next line';
is statements("=formula\n\n  x  y\n"), formula('x y'), 'blank lines skipped, blanks collapsed';
is statements("=formula a  b   c\n"), formula('a b c'), 'runs of blanks are one space';
is statements("=formula a b  \n"), formula('a b '), 'a trailing run of blanks is one space';
is statements("=formula B<x> y\n"), formula('B<x> y'), 'formatting codes are not read';
is statements("=formula\n=head1 T\n"), formula('=head1 T'), 'a directive on the next line is the text';
is statements("=formula x\nsay 1;\n").split(" | ")[0], formula('x'), 'the block stops after one line';
is statements("=formula x\nsay 1;\n").split(" | ").elems, 2, 'the next line is a statement';
is statements("say 0;\n=formula x\n\nsay 1;\n").split(" | ").elems, 3, 'between two statements';
is span("=formula E = mc^2\nsay 1;\n"), "0-17", 'range of the block';
is span("=formula\n\n  x  y\nsay 1;\n"), "0-16", 'range of the block with the text on a later line';
is (EVAL "=formula E = mc^2\n42"), 42, 'the line after an abbreviated formula runs';
