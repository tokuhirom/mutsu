use Test;
use lib 't/lib';
use ExportTestMod;
plan 1;

is exported-double(21), 42, 'is export makes sub available to caller';
