use Test;

# A Pod block whose directive is indented has a `margin`, the directive's
# indentation as written, and its lines are read with the margin taken off
# (#14880); rakudo 2026.09 shapes. Inside a block it is a statement of that
# block's list. An abbreviated paragraph ends at a line that is indented less
# than its directive, so the code after it runs (the parser used to swallow it).

plan 14;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub top($src) { $src.AST.statements[0] }
sub inner($src) { $src.AST.statements[0].expression.body.statement-list.statements[0] }

my $d = top("  =begin pod\n  x\n  =end pod\nsay 1;\n");
is $d.margin, "  ", 'delimited: the margin';
is $d.paragraphs.raku, '("x\n",)', 'delimited: the margin is taken off the lines';
is rng($d), "2-27", 'delimited: the range starts after the margin';

my $a = top("  =head1 T\n  more\n\nsay 1;\n");
is $a.margin, "  ", 'abbreviated: the margin';
is $a.paragraphs.raku, '("T\nmore\n\n",)', 'abbreviated: the paragraph keeps its blank line';
is rng($a), "2-17", 'abbreviated: the range';

my $n = inner("sub g \{\n    =begin pod\n    x\n    =end pod\n    1\n}\n");
is $n.margin, "    ", 'inside a sub: the margin';
is rng($n), "12-41", 'inside a sub: the range';
is inner("sub g \{\n    =begin pod\n    x\n    =end pod\n    1\n}\n").paragraphs.raku, '("x\n",)', 'inside a sub: the paragraph';

# The line after an indented abbreviated paragraph is code, not Pod.
my $items = "sub h \{\n  =item a\n  bb\nsay 7;\n}\n".AST.statements[0].expression.body.statement-list;
is $items.statements.elems, 2, 'a less indented line ends the paragraph (a block and a statement)';
is $items.statements[0].paragraphs.raku, '("a\nbb\n",)', 'the paragraph';

is top("=begin pod\ntext\n  indented continuation\nmore\n=end pod\n").paragraphs.raku,
    '("text\nindented continuation\nmore\n",)', 'a pod block drops the blanks of an indented continuation';
is top("=begin foo\ntext\n  indented continuation\nmore\n=end foo\n").paragraphs.raku,
    '("text\n  indented continuation\nmore\n",)', 'a custom block keeps them';
is "  =head1 a\nsay 42;\n".AST.statements.elems, 2, 'code after an indented paragraph at the start of the unit is kept';
