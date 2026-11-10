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
    token statement-control:sym<compile-outer> {
        'compile-outer' <.end-keyword>
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
    # An action that compiles a routine assigning to a variable of the importing unit,
    # which the slang host cannot see.
    method statement-control:sym<compile-outer>(Mu $/) {
        my $sub = RakuAST::Sub.new(
            body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$outer'),
                    infix => RakuAST::Infix.new('='),
                    right => RakuAST::IntLiteral.new(1))))));
        my $compiled = try { $sub.to-begin-time($*R, $*CU.context); $sub.meta-object };
        remember-it($compiled.defined ?? 'compiled' !! 'not compiled');
        self.attach: $/, RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(1));
    }
}

use Slangify EffectGrammar, EffectActions;
