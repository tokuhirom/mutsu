use Test;

# The `term` of an `enum` declaration carries the ranges of the text it was
# written as (#15780): a parenthesised list of pairs (`Parentheses`,
# `SemiList`, its statement, the comma list and each `FatArrow`) and a `<...>`
# word quote (`QuotedString` over the brackets, its `StrLiteral` inside them).

plan 4;

use experimental :rakuast;

sub origins(Str $src) {
    my @l;
    sub w($n) { @l.push($n.^name.split('::').tail ~ "@" ~ .from ~ "-" ~ .to) with $n.origin; $n.visit-children(&w) }
    w($src.AST);
    @l.grep(/^[Parentheses | SemiList | Expression | ApplyListInfix | Infix | FatArrow | QuotedString | StrLiteral]/).join(' ');
}

is origins('enum Ek (a1 => 1, b1 => 2);'),
    'Expression@0-26 Parentheses@8-26 SemiList@9-25 Expression@9-25 ApplyListInfix@9-25 Infix@16-17 FatArrow@9-16 FatArrow@18-25',
    'a parenthesised list of pairs';
is origins('enum Ej <p q>;'), 'Expression@0-13 QuotedString@8-13 StrLiteral@9-12', 'a word quote';
is origins('my enum Ed <m n>;'), 'Expression@0-16 QuotedString@11-16 StrLiteral@12-15', 'with a scope declarator';
is origins('our enum El (a1 => 1, b1 => 2) ;').words.elems > 4, True, 'trailing whitespace before the semicolon';
