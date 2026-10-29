use lib 't/lib';
use Test;
use CuProbeExport;

plan 4;

# `$*CU` is the RakuAST::CompUnit of the unit being compiled: visible to BEGIN
# code and to what `use` runs while the unit compiles, a Failure at run time.
# Found via static::CLI (#14972), whose EXPORT sub reads it.

my $begin-name;
BEGIN $begin-name = $*CU.^name;

is $begin-name, 'RakuAST::CompUnit', 'BEGIN sees the CompUnit';
is cu-seen(), 'RakuAST::CompUnit/RakuAST::StatementList',
    'an imported EXPORT sub sees the importing unit';
ok $*CU ~~ Failure, 'at run time $*CU is a Failure';
my $first;
my $second;
BEGIN { $first = $*CU.WHICH; $second = $*CU.WHICH }
is $first, $second, 'the compile-time CompUnit is one object per unit';
