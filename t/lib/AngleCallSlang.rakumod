# Fixture for t/lang/parsing/slang-arglist-infix-stopper.t (#16888, EEL 0.3's
# `<name: args>` calls): the grammar role overrides the core `arglist` with one
# that sets `$*IN_REGEX_ASSERTION`, so a `>` ends the arguments, and the core
# `circumfix:sym<( )>` with one that resets it, so a parenthesized comparison
# keeps its `>`.
use nqp;

sub cap(Mu $m, Str $key) { nqp::atkey($m.hash, $key) }

role AngleCallGrammar[&regular-arglist, &regular-paren] {
    method arglist(|args) {
        my $*IN_REGEX_ASSERTION = True;
        regular-arglist(self, |args)
    }
    method circumfix:sym<( )>(|args) {
        my $*IN_REGEX_ASSERTION = False;
        regular-paren(self, |args)
    }
    token statement-control:sym<call> {
        'call' <.kok> '<' $<name>=[\w+] ':' <.ws> <arglist> <.ws> '>'
    }
}

role AngleCallActions {
    method statement-control:sym<call>(Mu $/) {
        my $call = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier(cap($/, 'name').Str),
            args => cap($/, 'arglist').ast,
        );
        make RakuAST::Statement::Expression.new(expression => $call);
    }
}

sub EXPORT(|) {
    my $grammar = $*LANG.slang_grammar('MAIN');
    my $arglist := $grammar.HOW.find_method($grammar, 'arglist');
    my $paren := $grammar.HOW.find_method($grammar, 'circumfix:sym<( )>');
    my &arglist = -> Mu \cursor, |args { $arglist(cursor, |args) };
    my &paren = -> Mu \cursor, |args { $paren(cursor, |args) };
    $*LANG.define_slang(
        'MAIN',
        $grammar.^mixin(AngleCallGrammar[&arglist, &paren]),
        $*LANG.slang_actions('MAIN').^mixin(AngleCallActions),
    );
    %( )
}
