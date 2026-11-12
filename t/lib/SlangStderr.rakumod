use experimental :rakuast;

our role StderrGrammar {
    token statement-control:sym<dp> {
        'dp' { note 'from the token' } <.end-keyword> <.ws> $<block>=<block>
    }
}

our role StderrActions {
    method statement-control:sym<dp>(Mu $/) {
        note 'from the action';
        $*ERR.say('from the handle');
        self.attach: $/, RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(1));
    }
}

sub EXPORT() {
    $*LANG.define_slang(
        'MAIN',
        $*LANG.slang_grammar('MAIN').^mixin(StderrGrammar),
        $*LANG.slang_actions('MAIN').^mixin(StderrActions),
    );
    Map.new
}
