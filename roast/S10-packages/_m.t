use Test;
use lib "packages/LoadFromInside/lib";
eval-lives-ok q[use LoadMe], "a";
plan 1;
