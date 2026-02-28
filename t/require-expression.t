my $required-Test = (require Test <&plan &is &eval-lives-ok>);

use lib "roast/packages/S11-modules/lib";

plan 3;

is $required-Test.gist, "(Test)", "require package expression returns a package object";
is (require "InnerModule.rakumod"), "InnerModule.rakumod", "require string expression returns the same string";
eval-lives-ok q|my $x; BEGIN try EVAL '$x = Test'; die unless $x.gist eq "(Test)";|,
    "require installs package stub visible to BEGIN EVAL";
