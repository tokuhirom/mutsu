# Fixture for t/lang/parsing/slang-action-effect-replay.t (#17243): a slang whose action
# does more than attach a node -- it records the statement in module-level state, as
# Qwiratry::Mold::Slang's `mold` registers into a registry.
use v6.e.PREVIEW;

my @COLLECTED;
sub collected() is export { @COLLECTED }
sub remember-it(Str $what) { @COLLECTED.push($what) }

our role EffectGrammar {
    token statement-control:sym<remember> {
        'remember' <.end-keyword> <.ws> $<what>=<identifier>
    }
}

our role EffectActions {
    method statement-control:sym<remember>(Mu $/) {
        remember-it(~$<what>);
        self.attach: $/, RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('diag'),
                args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('replayed ' ~ $<what>)),
            ),
        );
    }
}

use Slangify EffectGrammar, EffectActions;
