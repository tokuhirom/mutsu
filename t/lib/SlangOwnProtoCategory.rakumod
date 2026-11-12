# Fixture for t/routines/dispatch/slang-own-proto-category.t: a slang whose
# production calls a proto category the slang declares itself (`tally-op`, with
# `:sym<+>` / `:sym<->` candidates), as Polyglot::Brainfuck's `BF-command`.
use v6.e.PREVIEW;
our role Grammar {
    proto token tally-op {*}
    token tally-op:sym<+> { <sym> }
    token tally-op:sym<-> { <sym> }
    token statement-control:sym<tally> {
        'tally' <.end-keyword> <.ws> $<ops>=[<tally-op>+]
    }
}
our role Actions {
    method statement-control:sym<tally>(Mu $/) {
        my $ops = $/.Str.substr(5);
        my $n = $ops.comb('+').elems - $ops.comb('-').elems;
        my $call = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('say'),
            args => RakuAST::ArgList.new(RakuAST::IntLiteral.new($n)),
        );
        self.attach: $/, RakuAST::Statement::Expression.new(expression => $call);
    }
}
sub EXPORT() {
    my $LANG := $*LANG;
    $LANG.define_slang(
        'MAIN',
        $LANG.slang_grammar('MAIN').^mixin(Grammar),
        $LANG.slang_actions('MAIN').^mixin(Actions),
    );
    BEGIN Map.new
}
