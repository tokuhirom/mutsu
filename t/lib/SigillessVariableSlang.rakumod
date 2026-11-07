# Fixture for t/lang/parsing/slang-sigilless-variable.t: a slang whose
# `sigilless-variable` token accepts an identifier or one Symbol-other character
# (Slang::Nogil / Slang::Emoji).
use v6.e.PREVIEW;

our role Names {
    token sigilless-variable { <.ident>+ | <.:So> }
}

sub EXPORT() {
    $*LANG.refine_slang('MAIN', Names, role { });
    BEGIN Map.new
}
