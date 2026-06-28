use Test;

# Perl 5 unary functions (`ord`/`chr`/`lc`/`uc`/`abs`) defaulted to `$_` in
# Perl 5 but require an explicit argument/invocant in Raku. A bare use with no
# argument is X::Obsolete (Rakudo: «Unsupported use of bare "ord"...»).
# Regression: S32-exceptions/misc.t ("adequate error message when calling bare
# ord"). A real call or method form must still work.

plan 13;

for <ord chr lc uc abs> -> $fn {
    throws-like $fn ~ '.Cool', X::Obsolete, "bare $fn (followed by .method) is X::Obsolete";
}

# real calls / listops / method forms are unaffected
is ord('A'), 65, 'ord with parens works';
is lc('HI'), 'hi', 'lc with parens works';
is (uc 'hi'), 'HI', 'uc as listop works';
is abs(-5), 5, 'abs with parens works';
is (chr 66), 'B', 'chr as listop works';
is "A".ord, 65, '.ord method works';
is "HI".lc, 'hi', '.lc method works';
is (-5).abs, 5, '.abs method works';
