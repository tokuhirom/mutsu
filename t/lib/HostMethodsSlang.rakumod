# Fixture for t/lang/parsing/slang-host-methods.t (ADR-16888): a slang whose
# grammar role overrides the core `arglist` and `block` methods with ones that
# delegate through `HOW.find_method`, and whose
# tokens call `<.kok>`, `<arglist>`, `<statement>` and `<.set_braid_from(self)>`.
use v6.e.PREVIEW;
use nqp;

sub cap(Mu $m, Str $key) { nqp::atkey($m.hash, $key) }

role HostMethodsGrammar[&regular-arglist] {
    method arglist(|args) {
        my $*SEEN-ARGLIST = True;
        regular-arglist(self, |args)
    }
    token statement-control:sym<shout> {
        'shout' <.kok> <.ws> '(' <arglist> ')' <.set_braid_from(self)>
    }
    token statement-control:sym<justdo> {
        'justdo' <.kok> <statement>
    }
}

role HostMethodsActions {
    method statement-control:sym<shout>(Mu $/) {
        my $call = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('capture'),
            args => cap($/, 'arglist').ast,
        );
        make RakuAST::Statement::Expression.new(expression => $call);
    }
    method statement-control:sym<justdo>(Mu $/) {
        make cap($/, 'statement').ast;
    }
}

sub EXPORT(|) {
    my $grammar = $*LANG.slang_grammar('MAIN');
    my $method := $grammar.HOW.find_method($grammar, 'arglist');
    my &core = -> Mu \cursor, |args { $method(cursor, |args) };
    $*LANG.define_slang(
        'MAIN',
        $grammar.^mixin(HostMethodsGrammar[&core]),
        $*LANG.slang_actions('MAIN').^mixin(HostMethodsActions),
    );
    %( )
}
