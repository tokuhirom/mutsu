use Test;
plan 5;

my $root = "_test_mkdir_rmdir_" ~ 1000000.rand.floor;
my $child = "$root/child";

ok mkdir($root), "mkdir creates root directory";
ok mkdir($child), "mkdir creates child directory";
nok rmdir($root), "rmdir returns false for non-empty directory";
ok rmdir($child), "rmdir removes child";
ok rmdir($root), "rmdir removes root after child is gone";
