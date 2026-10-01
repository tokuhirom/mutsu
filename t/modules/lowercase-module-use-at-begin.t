# A lowercase-named real module (`vars`, from the zef distribution of that
# name) is not a positional pragma: its `use` runs at BEGIN time, so a later
# BEGIN sees the symbols its `sub EXPORT` installed.
use lib 't/lib';
use Test;

BEGIN my @vars = <$frob @mung %seen>;
BEGIN plan 2 * @vars;

use vars @vars;

BEGIN ok ::{$_}:exists, "export for $_ is visible at BEGIN" for @vars;

ok $frob.VAR ~~ Scalar, '$frob is a Scalar container';
ok @mung ~~ Array, '@mung is an Array';
ok %seen ~~ Hash, '%seen is a Hash';
