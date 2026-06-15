use Test;
plan 1;
sub foo-ok() is test-assertion { flunk "foo-ok" }
foo-ok;
