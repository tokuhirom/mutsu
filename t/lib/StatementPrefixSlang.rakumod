# Fixture for t/lang/parsing/slang-statement-prefix.t (ADR-15762, #13370): a
# `statement-prefix:sym<>` token that reads a core `<blorst>` (a block, or one
# statement) after a keyword of its own, as Qwiratry's aggsub phasers do. The
# action answers a string describing the node it was given.
use v6.e.PREVIEW;
use nqp;

sub cap(Mu $m, Str $key) { nqp::atkey($m.hash, $key) }

role StatementPrefixGrammar {
    token statement-prefix:sym<TRACKED> {
        'TRACKED' <.kok> <blorst>
    }
}

role StatementPrefixActions {
    method statement-prefix:sym<TRACKED>(Mu $/) {
        my $node = cap($/, 'blorst').ast;
        self.attach: $/, RakuAST::StrLiteral.new($node.^name ~ ' ' ~ $node.raku.words.join(' '));
    }
}

sub EXPORT(|) {
    my $grammar = $*LANG.slang_grammar('MAIN');
    $*LANG.define_slang(
        'MAIN',
        $grammar.^mixin(StatementPrefixGrammar),
        $*LANG.slang_actions('MAIN').^mixin(StatementPrefixActions),
    );
    %( )
}
