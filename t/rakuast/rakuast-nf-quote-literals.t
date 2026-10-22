use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-10723 S10: `q:nfc'…'`, `q:nfd'…'`, `q:nfkc'…'`, `q:nfkd'…'` (S02,
# roast/S15-string-types/NF-types.t). Rakudo 2026.09 rejects the adverbs, so
# there is no oracle shape: mutsu reads the quote as a `QuotedString` whose one
# processor names the normalization form, and EVAL of that node produces the
# same `Uni` value as the quote.

plan 11;

is Q[q:nfc'abc'].AST.statements[0].expression.gist, q:to/END/.chomp, 'q:nfc reads as a quote with an nfc processor';
RakuAST::QuotedString.new(
  processors => <nfc>,
  segments   => (
    RakuAST::StrLiteral.new("abc"),
  )
)
END

for <nfc NFC nfd NFD nfkc NFKC nfkd NFKD> -> $adverb, $type {
    my $source = "q:{$adverb}'ẛ̣ ﬁ'";
    my $quoted = $source.AST.statements[0].expression;
    is-deeply $quoted.processors.List, ($adverb,), "$adverb: the processor names the form";
    my $round = EVAL $source.AST;
    my $direct = EVAL $source;
    is-deeply ($round.^name, $round.list), ($type, $direct.list),
        "$adverb: the round trip is the same $type value";
}

my $built = RakuAST::QuotedString.new(
    processors => <nfd>,
    segments   => (RakuAST::StrLiteral.new("\x[1E9B]\x[0323]"),),
);
my $value = EVAL $built;
isa-ok $value, NFD, 'a hand-built nfd quote is an NFD';
is-deeply $value.list, (0x17F, 0x323, 0x307), 'holding the decomposed codepoints';
