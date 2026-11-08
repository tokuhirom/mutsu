# Fixture for t/lang/parsing/slang-generated-constant.t (#13370): a slang
# production that installs a constant for the importing unit through
# `$*R.current-scope.merge-generated-lexical-declaration`, as Qwiratry's
# `aggsub` installs its class.
use v6.e.PREVIEW;

our role GeneratedConstantGrammar {
    token routine-declarator:sym<defconst> {
        'defconst' <.end-keyword> <.ws> $<name>=<.identifier> <.ws> $<value>=[\d+]
    }
}

our role GeneratedConstantActions {
    method routine-declarator:sym<defconst>(Mu $/) {
        my $decl := RakuAST::VarDeclaration::Implicit::Constant.new(
            :name(~$<name>), :value(+~$<value>), :scope('our'));
        $*R.current-scope.merge-generated-lexical-declaration($decl, :resolver($*R));
        self.attach: $/, RakuAST::Term::Name.new(RakuAST::Name.from-identifier('Nil'));
    }
}

use Slangify GeneratedConstantGrammar, GeneratedConstantActions;
