use Test;
# From Version::Repology t/03-p-is-patch: `::($name)` resolves the core terms.
plan 5;

is-deeply ::("True"), True, '::("True")';
is-deeply ::("False"), False, '::("False")';
is ::("Inf"), Inf, '::("Inf")';
is ::("NaN").raku, 'NaN', '::("NaN")';
sub f(:$p) { $p }
is-deeply f(:p(::("True"))), True, 'passes through a named arg';
