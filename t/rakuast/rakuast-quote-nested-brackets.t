use Test;

# A bracketed `q[...]` / `Q{...}` string holding a balanced inner bracket pair
# is read by rakudo as text up to the opener, a quote of the inner content, and
# the text from the closer on, recursively (#15780). Shapes and ranges measured
# on rakudo 2026.09.

plan 9;

use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

sub flat(Str $s) { $s.subst(/\s+/, ' ', :g) }
sub segs(Str $src) { flat($src.AST.statements[0].expression.raku) }
sub origins(Str $src) {
    my @l;
    sub w($n) { @l.push($n.^name.split('::').tail ~ "@" ~ .from ~ "-" ~ .to) with $n.origin; $n.visit-children(&w) }
    w($src.AST);
    @l.grep(/^[QuotedString | StrLiteral]/).join(' ');
}

is segs('Q[ab [c] d]'),
    'RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new("ab ["), RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new("c"), ) ), RakuAST::StrLiteral.new("] d"), ) )',
    'one inner pair';
is segs('q{x{y}z}'),
    'RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new("x\{"), RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new("y"), ) ), RakuAST::StrLiteral.new("}z"), ) )',
    'braces';
ok segs('Q[a [b [c] d] e]').contains('StrLiteral.new("b [")'), 'inner pairs nest';
ok segs('Q[a [b] c [d] e]').contains('StrLiteral.new("] c [")'), 'two inner pairs share the text between';
is segs('q[abc]'), 'RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new("abc"), ) )', 'no inner pair: one literal';
is segs('"p[q]r"'), 'RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new("p[q]r"), ) )', 'a double-quoted string is not split';
is origins('my $s = Q[ab [c] d];'),
    'QuotedString@8-19 StrLiteral@10-14 QuotedString@14-15 StrLiteral@14-15 StrLiteral@15-18', 'the ranges of the pieces';
is origins('Q[a []];'), 'QuotedString@0-7 StrLiteral@2-5 QuotedString@5-5 StrLiteral@5-5 StrLiteral@5-6', 'an empty inner pair';
is EVAL(Q[Q[ab [c [d] e] f]].AST), 'ab [c [d] e] f', 'EVAL joins the pieces back';
