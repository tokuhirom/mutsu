use experimental :rakuast;
use Test;

# Pod `table` blocks as RakuAST (#14880), measured on rakudo 2026.11:
# a cell that holds a formatting code is a `Doc::Paragraph`, a table
# written with non-ASCII blanks (NBSP) is read, and the configuration takes
# `[...]` / `{...}` values and the numbered `:34name` form.

plan 9;

sub blocks(Str $src) { $src.AST.statements.grep(RakuAST::Doc::Block) }

{
    my $t = blocks("=begin table\na | b\nx | y      Z<a comment> Z<more>\n=end table\n")[0];
    my @rows = $t.paragraphs;
    is-deeply @rows[0].cells, ("a", "b"), 'plain cells stay strings';
    my $cell = @rows[1].cells[1];
    isa-ok $cell, RakuAST::Doc::Paragraph, 'a cell with a formatting code is a paragraph';
    is $cell.atoms.map({ $_ ~~ Str ?? $_ !! .letter ~ '/' ~ .atoms.join }).join('|'),
        'y      |Z/a comment| |Z/more', 'its text and codes';
}

{
    # The first divider stringifies the rows before it before merging them.
    my $t = blocks("=begin table\n  a   I<b>\n  c   d\n  ------\n  e   f\n=end table\n")[0];
    is $t.paragraphs[0].cells.map(*.raku).join('|'), Q["a \nc "|"I<b>\nd"],
        'rows merged at the first divider are stringified';
}

{
    my $t = blocks("=begin table\n    Op  |  Meaning\n     \x[a0]+\x[a0]  |  set union\n=end table\n")[0];
    is $t.paragraphs.elems, 2, 'a table with NBSP blanks is read';
    is $t.paragraphs[1].cells.map(*.raku).join('|'), Q[" "|" "|" set union"],
        'NBSP is a blank around a divider';
}

{
    my $t = blocks("=begin table :k4[\"str\"] :k5[+1e4]\nfoo\n=end table\n")[0];
    is $t.config<k4>.^name, 'RakuAST::Circumfix::ArrayComposer', 'a [...] value';
    is $t.config<k5>.^name, 'RakuAST::Circumfix::ArrayComposer', 'a [...] number';
}

{
    my $t = blocks("=for info :034foo\nx\n\n")[0];
    is $t.config<foo>.raku, 'RakuAST::IntLiteral.new(34)', ':34name is name(34)';
}
