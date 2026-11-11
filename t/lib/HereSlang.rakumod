# Fixture for t/regex/match/slang-match-orig-is-unit.t (#16888, EEL 0.3's
# `node()` placing a definition): `here-pos` stands for the line and column its
# match starts at, computed from the cursor's `.orig` and `.from` as EEL does,
# and `orig-chars` for the length of the `.orig` it saw.
use nqp;

role HereGrammar {
    token term:sym<here> { 'here-pos' <.end-keyword> }
    token term:sym<orig-chars> { 'orig-chars' <.end-keyword> }
}

role HereActions {
    method term:sym<here>(Mu $/) {
        my $prefix = $/.orig.substr(0, $/.from);
        my $line = $prefix.comb("\n").elems + 1;
        my $column = $prefix.chars - ($prefix.rindex("\n") // -1);
        make RakuAST::StrLiteral.new("$line:$column");
    }
    method term:sym<orig-chars>(Mu $/) {
        make RakuAST::IntLiteral.new($/.orig.chars);
    }
}

sub EXPORT(|) {
    $*LANG.define_slang(
        'MAIN',
        $*LANG.slang_grammar('MAIN').^mixin(HereGrammar),
        $*LANG.slang_actions('MAIN').^mixin(HereActions),
    );
    %( )
}
