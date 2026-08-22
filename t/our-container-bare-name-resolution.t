use Test;
use lib 't/lib';
use UnitOurContainer;

# A module's `our @arr` / `our %h` is a PACKAGE variable of that module. Its
# own routines reference it by the bare name `@arr`, because a sub body
# compiles under a mangled `Pkg::&sub/arity` state-scope package that disables
# package-qualification. Resolved against `env` alone, that bare name found
# whatever `@arr` the LOADING script had declared, so every module routine
# mutated the script's array instead of its own -- while the package-qualified
# mirror `@UnitOurContainer::arr` sat there holding the correct value nobody
# consulted.
#
# See todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md
# item 1 and ADR-0039 sec 4.1 (which excluded `our` from slice 1 precisely
# because it needs a resolution fix, not a store).

plan 28;

# The script declares its own same-named containers -- the collision setup.
my @arr = <x y z>;
my %h   = (own => 1);

# --- @: read ---------------------------------------------------------------
is arr-read(), "a,b", "module sees its own our @arr, not the script's";
is @arr.join(","), "x,y,z", "script's my @arr is untouched by the module's read";
is @UnitOurContainer::arr.join(","), "a,b", "package-qualified mirror agrees";
is arr-elems(), 2, "module's our @arr has its own element count";

# --- @: push ---------------------------------------------------------------
arr-push("c");
is arr-read(), "a,b,c", "the module's push lands on its own our @arr";
is @arr.join(","), "x,y,z", "the module's push does not reach the script's my @arr";
is @UnitOurContainer::arr.join(","), "a,b,c", "the mirror observes the push";

# --- @: element read / element assign --------------------------------------
is arr-elem(1), "b", "module indexes its own our @arr";
arr-set(0, "Z");
is arr-read(), "Z,b,c", "the module's element-assign lands on its own our @arr";
is @arr.join(","), "x,y,z", "the module's element-assign does not reach the script";

# --- @: pop ----------------------------------------------------------------
is arr-pop(), "c", "the module pops from its own our @arr";
is arr-read(), "Z,b", "the module's our @arr shrank";
is @arr.join(","), "x,y,z", "the script's my @arr still has all three elements";

# --- @: a block nested inside a module routine -----------------------------
is arr-push-in-block("n"), "Z,b,n", "a block inside a module routine sees the package container";
is @arr.join(","), "x,y,z", "the nested-block push does not reach the script";

# --- %: read / key-set / delete --------------------------------------------
is hash-read(), "k=v", "module sees its own our %h, not the script's";
is %h.keys.sort.join(","), "own", "script's my %h is untouched";
is %UnitOurContainer::h.keys.sort.join(","), "k", "package-qualified hash mirror agrees";

hash-set("k2", "v2");
is hash-read(), "k=v,k2=v2", "the module's key-set lands on its own our %h";
is %h.keys.sort.join(","), "own", "the module's key-set does not reach the script's my %h";
is hash-elem("k2"), "v2", "module reads back the key it just set";

is hash-delete("k"), "v", ":delete returns the removed value from the module's own our %h";
is hash-read(), "k2=v2", "the module's our %h lost the deleted key";
is %h.keys.sort.join(","), "own", "the script's my %h is unaffected by the module's :delete";

# --- lexical shadowing inside the module still wins ------------------------
# A routine-local `my @arr` is a different variable from the package `our @arr`.
# Preferring the package mirror must NOT override a genuine lexical declaration.
is shadowed-local(), "p,q,r", "a routine-local my @arr shadows the package our @arr";
is arr-read(), "Z,b,n", "the package our @arr is untouched by the shadowing routine";
is shadowed-local-hash(), "extra,own", "a routine-local my %h shadows the package our %h";
is hash-read(), "k2=v2", "the package our %h is untouched by the shadowing routine";
