use v6;
use Test;
use lib 't/lib';
use ExportEnumTerms;

# #16562: `MY::NAME` selects the importing scope and reads the term `NAME`
# there: an enum value or a constant a `use` imported. It died with
# "Could not find symbol '&EXA' in 'GLOBAL::MY'" before.

plan 7;

is MY::EXA.raku, 'ExportEnumTerms::Code::EXA', 'an imported enum value';
is MY::EXA.Int, 2, 'its value';
is MY::EXB + 1, 12, 'in arithmetic';
ok MY::EXA, 'in boolean context';
is MY::EXPORT-LIMIT, 99, 'an imported constant';
is UNIT::EXPORT-LIMIT, 99, 'through UNIT::';

constant LOCAL = 7;
is MY::LOCAL, 7, 'a constant of the scope itself';
