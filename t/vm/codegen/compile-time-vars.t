use Test;
plan 2;

ok $?FILE.defined, '$?FILE is defined';
ok $?LINE.defined, '$?LINE is defined';
