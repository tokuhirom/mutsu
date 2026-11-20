use experimental :rakuast;
use Test;

# A colonpair word in a `<< >>` / `« »` quote is a `QuoteWordsAtom` of its own
# and the text around it (blanks and plain words) stays `StrLiteral`
# segments, as rakudo 2026.11 has it (#18299) — in an expression and as an
# enum's body. `EVAL` of the tree builds the same list.

plan 12;

sub segments($q) {
    $q.segments.map({
        $_ ~~ RakuAST::StrLiteral ?? .value.raku !! .^name.subst('RakuAST::', '') ~ '(' ~ .atom.^name.subst('RakuAST::', '') ~ ')'
    }).join(' | ')
}
sub arg(Str $src) { $src.AST.statements[0].expression.args.args[0] }

is segments(arg(Q[say << a :b<c> d >>])), '" a " | QuoteWordsAtom(ColonPair::Value) | " d "', 'words around a pair';
is segments(arg(Q[say <<:b<c>>>])), 'QuoteWordsAtom(ColonPair::Value)', 'a pair alone';
is segments(arg(Q[say << :!b :c("x y") >>])),
    '" " | QuoteWordsAtom(ColonPair::False) | " " | QuoteWordsAtom(ColonPair::Value) | " "',
    'a negated pair and a pair with a quoted value';
is segments(arg(Q[say << :b >>])), '" " | QuoteWordsAtom(ColonPair::True) | " "', 'a bare pair';
is segments(arg(Q[say << a b >>])), '" a b "', 'plain words stay one segment';
is arg(Q[say << :ms<time> >>]).segments[1].atom.value.processors.join(' '), 'words val',
    'a pair\'s angle value is a word quote';

is Q[say << a :b<c> d >>].AST.DEPARSE, "say << a :b<c> d >>\n", '.DEPARSE writes it back';
is-deeply EVAL(RakuAST::StatementList.new(RakuAST::Statement::Expression.new(
    expression => arg(Q[say << a :b<c> d >>])))), ("a", :b("c"), "d"), 'EVAL builds the list';

my $enum = Q[our Str enum InlineUnits << :ms<time> :s<time> >>;].AST;
is $enum.DEPARSE, "our Str enum InlineUnits << :ms<time> :s<time> >>\n", 'an enum body keeps its spelling';
is segments($enum.statements[0].expression.term),
    '" " | QuoteWordsAtom(ColonPair::Value) | " " | QuoteWordsAtom(ColonPair::Value) | " "',
    'and its atoms';
EVAL Q[our Str enum IU << :ms<time> :s<time> x >>; our $seen = (IU::ms.value, IU::s.value, IU::x.value)].AST;
is-deeply $OUR::seen, ("time", "time", "timf"), 'EVAL of the enum keeps its values';
EVAL Q[our enum IV << :ms(1) y >>; our $yv = IV::y.value].AST;
is $OUR::yv, 2, 'a numeric pair counts on';
