use Test;

# A unit's `EXPORT` package is a lexical of the unit: `EXPORT::DEFAULT::`
# lists the unit's own exports only, not those of a package-less module it
# uses (Test.rakumod's `&ok`, ...), and `MY::<EXPORT>` exists at the unit's
# root alone (#13854). Checked against rakudo.
plan 6;

sub mine is export { 1 }
sub tagged is export(:extra) { 2 }

is-deeply EXPORT::DEFAULT::.keys.sort.List, ('&mine',), 'the DEFAULT stash lists the unit\'s own export';
is-deeply EXPORT::ALL::.keys.sort.List, ('&mine', '&tagged'), 'so does ALL';
ok MY::<EXPORT>:exists, 'MY::<EXPORT> exists at the unit root';
is MY::<EXPORT>.^name, 'EXPORT', 'it is the EXPORT package';
{
    nok MY::<EXPORT>:exists, 'not in a block';
}
sub in-routine { MY::<EXPORT>:exists }
nok in-routine(), 'nor in a routine';
