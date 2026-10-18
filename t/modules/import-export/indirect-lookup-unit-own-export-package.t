use Test;

# `::('EXPORT::...')` names the running unit's own export package, a lexical
# of the unit in rakudo (#13808). Checked against rakudo.
plan 3;

sub mine is export { 1 }
sub tagged is export(:extra) { 2 }

is ::('EXPORT::DEFAULT::&mine')(), 1, 'the unit\'s DEFAULT export, looked up indirectly';
is ::('EXPORT::extra::&tagged')(), 2, 'a tagged export';
ok EXPORT::DEFAULT::<&mine>:exists, 'the DEFAULT stash holds it';
