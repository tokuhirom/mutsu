# Fixture for t/grammar/method-table-token-source.t (Grammar::TokenProcessing).
unit module GrammarTokenSource;

role Inner {
    proto token noun {*}
    token noun:sym<English> { 'cat' | 'dog' }
}

role Outer does Inner {
    rule phrase { 'the' <noun> }
}

grammar Words is export does Outer {
    rule TOP { <phrase> }
}
