use Test;
use lib 't/lib';
use UnitFileLexical;

# A module's file-scope `my` is lexical to its compunit. mutsu runs a module
# body in the env of whatever frame loaded it, so a plain env key made both
# variables ONE storage cell and writes went both ways: the module could not
# see its own initializer once the script declared the same name, and the
# module's own assignment silently replaced the script's value.
# See todo/deep/module-file-scope-my-shares-the-callers-env.md.

plan 21;

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

# ADR-0039 slice 1: the same isolation for `@`/`%` file-scope lexicals. A
# 15-assertion operation matrix over read / push / element-assign /
# whole-assign / key-set / `:delete`, for both `@items` and `%items`, with the
# script declaring its OWN same-named containers throughout.
my @items = <x y z>;
my %items = (x => 10, y => 20);

is arr-read(), "a,b", "module @items initial read, unaffected by script's my @items";
is hash-read(), "a=1,b=2", "module %items initial read, unaffected by script's my %items";

arr-push("c");
is arr-read(), "a,b,c", "module @items sees its own push";
is @items.join(","), "x,y,z", "script @items untouched by the module's push";

arr-elem-assign(0, "Z");
is arr-read(), "Z,b,c", "module @items sees its own element-assign";
is @items.join(","), "x,y,z", "script @items untouched by the module's element-assign";

arr-whole-assign();
is arr-read(), "p,q", "module @items sees its own whole-assign";
is @items.join(","), "x,y,z", "script @items untouched by the module's whole-assign";

hash-key-set("a", 100);
is hash-read(), "a=100,b=2", "module %items sees its own key-set";
is %items.sort(*.key).map({ "{.key}={.value}" }).join(","), "x=10,y=20",
    "script %items untouched by the module's key-set";

hash-delete("b");
is hash-read(), "a=100", "module %items sees its own :delete";
is %items.sort(*.key).map({ "{.key}={.value}" }).join(","), "x=10,y=20",
    "script %items untouched by the module's :delete";

@items.push("Q");
is @items.join(","), "x,y,z,Q", "script's own push does not reach the module's @items";
is arr-read(), "p,q", "module @items unaffected by the script's own push";

%items{"z"} = 30;
is hash-read(), "a=100", "module %items unaffected by the script's own key-set";
