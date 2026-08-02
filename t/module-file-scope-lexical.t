use Test;
use lib 't/lib';
use UnitFileLexical;

# A module's file-scope `my` is lexical to its compunit. mutsu runs a module
# body in the env of whatever frame loaded it, so a plain env key made both
# variables ONE storage cell and writes went both ways: the module could not
# see its own initializer once the script declared the same name, and the
# module's own assignment silently replaced the script's value.
# See todo/deep/module-file-scope-my-shares-the-callers-env.md.

plan 6;

my $secret = "script";

is peek(), "module", "module sees its own scalar, not the script's";
is $secret, "script", "script keeps its own scalar";

poke("poked");
is peek(), "poked", "the module's write is visible to the module";
is $secret, "script", "the module's write does not reach the script's scalar";

# Lazy initialization from inside a module sub is the shape that made
# Test.rakumod's `_init_io` overwrite a test file's own `my $output`.
my $lazy = "";
is lazy-init(), "inited", "a module sub initializes its own lexical";
is $lazy, "", "the lazy init does not reach the script's same-named lexical";
