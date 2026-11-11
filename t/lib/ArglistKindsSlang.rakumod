# Fixture for t/lang/parsing/slang-arglist-whatever-argument.t (#16888, EEL
# 0.3's index descriptors): `%%kinds(ARGS)` reads its arguments with the core
# `<arglist>` and stands for the string of what each colonpair value's
# expression holds, so the test sees the nodes the slang's action saw.
use nqp;

sub cap(Mu $m, Str $key) { nqp::atkey($m.hash, $key) }

sub describe($value) {
    my $e = $value ~~ RakuAST::Circumfix::Parentheses
        ?? $value.semilist.statements[0].expression !! $value;
    given $e {
        when RakuAST::ApplyPostfix { 'postfix:' ~ .operand.^name }
        when RakuAST::ApplyInfix { 'infix:' ~ .left.^name }
        default { .^name }
    }
}

# What an operator's lookup resolved to, and a literal argument's value.
sub resolved($value) {
    my $e = $value ~~ RakuAST::Circumfix::Parentheses
        ?? $value.semilist.statements[0].expression !! $value;
    given $e {
        when RakuAST::ApplyInfix {
            my $r = .infix.resolution;
            .infix.is-resolved
                ?? join('/', $r.lexical-name, ($r ~~ RakuAST::CompileTimeValue),
                    ($r ~~ RakuAST::Declaration::External::Setting))
                !! 'unresolved'
        }
        when RakuAST::ApplyPostfix {
            .postfix.args.args[0].maybe-compile-time-value
        }
        default { .^name }
    }
}

role ArglistKindsGrammar {
    token term:sym<resolved> {
        '%%resolved(' <arglist> ')'
    }
    token term:sym<kinds> {
        '%%kinds(' <arglist> ')'
    }
}

role ArglistKindsActions {
    method term:sym<resolved>(Mu $/) {
        my @r = cap($/, 'arglist').ast.args.map({ resolved(.value) });
        make RakuAST::StrLiteral.new(@r.join(' '));
    }
    method term:sym<kinds>(Mu $/) {
        my @kinds = cap($/, 'arglist').ast.args.map({ describe(.value) });
        make RakuAST::StrLiteral.new(@kinds.join(' '));
    }
}

sub EXPORT(|) {
    $*LANG.define_slang(
        'MAIN',
        $*LANG.slang_grammar('MAIN').^mixin(ArglistKindsGrammar),
        $*LANG.slang_actions('MAIN').^mixin(ArglistKindsActions),
    );
    %( )
}
