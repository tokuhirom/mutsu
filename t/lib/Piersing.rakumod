# Fixture for t/slang-piersing-activation.t, mirroring Slangify's own
# upstream t/Piersing.rakumod fixture: a slang whose role overrides the
# `identifier`/`name` grammar rules so a bare identifier may end in a
# trailing `?`/`!` (e.g. `sub pass?(|c) { ... }`, called as `pass? "..."`).
my role Piersing {
    token identifier { <ident> [ <.apostrophe> <.ident> ]* <[?!]>? }
    token name       { [ | <identifier> <morename>* | <morename>+ ] <[?!]>? }
}

use Slangify Piersing, Mu;
