# Fixture for t/lang/parsing/slang-anon-lambda.t (ADR-15762, #13370): the shape
# of Slang::Mosdef. A `routine-declarator:sym<sub>` token with keywords of its
# own (`lambda`, `λ`) reads the core definition with `key-origin`; the slang has
# no actions role, so the production stands for the routine it captured.
use v6.e.PREVIEW;

my role LegacyGrammar {
    rule routine_declarator:sym<sub> {
        [ 'sub' | 'lambda' | 'λ' ] <routine_def('sub')>
    }
}

my role LambdaGrammar {
    token routine-declarator:sym<sub> {
        [ <.routine-sub> | lambda | 'λ' ]
        <.end-keyword>
        <routine-def=.key-origin('routine-def', 'sub')>
    }
}

use Slangify LambdaGrammar, Mu, LegacyGrammar, Mu;
