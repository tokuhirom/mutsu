# Fixture for t/grammar/slang-helper-token-and-grammar-role.t (#16888): a
# slang registered through `define_slang` whose parametric `Grammar[...]` role has a
# helper token (`eel-word`) that only its own production calls, as EEL's
# `EEL::Slang::Grammar` has many `eel-*` rules.
use v6.e.PREVIEW;
our role Grammar[Mu $regular] {
    token eel-word { <[a..z]>+ }
    token statement-control:sym<shout> { 'shout' <.end-keyword> <.ws> $<w>=<eel-word> ';'? }
}
our role Actions {
    method statement-control:sym<shout>(Mu $/) {
        my $call = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('say'),
            args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('SHOUT')),
        );
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $call);
    }
}
sub EXPORT() {
    my $LANG := $*LANG;
    my $grammar = $LANG.slang_grammar('MAIN');
    my &regular = -> Mu $cursor { 1 };
    my $actions = $LANG.slang_actions('MAIN').^mixin(Actions);
    $LANG.define_slang('MAIN', $grammar.^mixin(Grammar[&regular]), $actions);
    BEGIN Map.new
}
