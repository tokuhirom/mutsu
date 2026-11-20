# A test localization in the shape every L10N::XX distribution generates for
# `RakuAST::Deparse::L10N::XX`: a role whose `xsyn` translates a keyword by its
# `prefix-key` name (see t/rakuast/rakuast-deparse-l10n-language.t).

my constant %xlation = "scope-my", "mein", "block-if", "wenn", "block-else", "sonst",
  "core-say", "sag", "block-for", "fuer", "routine-sub", "unter", "core-return", "zurueck",
  "infix-xor", "xoder", "package-class", "klasse", "scope-has", "hat", "routine-method", "methode",
  "term-self", "selbst";
role RakuAST::Deparse::L10N::XT {
    method xsyn (str $prefix, str $key) {
        %xlation{"$prefix-$key"} // $key
    }
}
