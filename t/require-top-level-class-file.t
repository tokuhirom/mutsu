use Test;

plan 2;

use lib 'roast/packages/S11-modules/lib';

require "GlobalOuter.rakumod";

is ::('GlobalOuter').^name, 'GlobalOuter', 'top-level class required from file keeps its original name';
ok ::('GlobalOuter').load, 'required top-level class keeps its methods';
