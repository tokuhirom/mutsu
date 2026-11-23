use Test;

# `Paragraph.from-string` of a formatting code whose closer never comes dies
# as on rakudo, naming the innermost such code and its closer (#19040).

plan 10;

sub dies-with($text, $message, $desc) {
    throws-like { RakuAST::Doc::Paragraph.from-string($text) }, Exception,
        :$message, $desc;
}

dies-with "a B<b c\n", "RakuDoc markup code B missing endtag '>'.", 'an unterminated B<';
dies-with "a B<b C<d> c\n", "RakuDoc markup code B missing endtag '>'.", 'a closed code inside it';
dies-with "a B<x C<y\n", "RakuDoc markup code C missing endtag '>'.", 'the innermost unterminated code';
dies-with "a B<b> C<c\n", "RakuDoc markup code C missing endtag '>'.", 'after a closed code';
dies-with "a B<<b c\n", "RakuDoc markup code B missing endtag '>>'.", 'a double-angle code';
dies-with "a B«b c\n", "RakuDoc markup code B missing endtag '»'.", 'a guillemet code';
dies-with "a B<<<b c\n", "RakuDoc markup code B missing endtag '>>>'.", 'a triple-angle code';
dies-with "a L<x|y c\n", "RakuDoc markup code L missing endtag '>'.", 'a code with metadata';

throws-like {
    RakuAST::Doc::Block.from-paragraphs(:type<pod>, :paragraphs("a B<b c\n",))
}, Exception, message => "RakuDoc markup code B missing endtag '>'.",
    'from-paragraphs dies the same way';

is RakuAST::Doc::Paragraph.from-string("a B<b> c\n").atoms.elems, 3,
    'a terminated code still reads';
