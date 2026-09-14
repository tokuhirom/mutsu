use Test;

plan 1;

use lib 't/lib';
use CustomExportUseArgsFixture <one two>;

is custom-export-args(), 'one,two',
    'sub EXPORT receives use arguments instead of treating them as export tags';
