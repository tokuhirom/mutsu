# Fixture for t/lang/parsing/slang-host-raku-class-name.t: a slang whose action
# tells the Raku grammar from the legacy one the way Slang::Roman and
# Slang::Date do, by the name of its own class (`Raku::...` under RakuAST).
use v6.e.PREVIEW;
use experimental :rakuast;

my role Grammar {
    token number:sym<tripled> { '0t' <( \d+ }
}

my role Actions {
    method number:sym<tripled>(Mu $/) {
        if self.^name.starts-with('Raku::') {
            make RakuAST::IntLiteral.new($/.Str.Int * 3);
        }
        else {
            die "not running under the Raku grammar: {self.^name}";
        }
    }
}

sub EXPORT() {
    $*LANG.refine_slang('MAIN', Grammar, Actions);
    BEGIN Map.new
}
