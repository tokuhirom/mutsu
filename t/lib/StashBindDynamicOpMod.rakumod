# Fixture for t/modules/import-export/export-stash-our-bind-computed.t:
# operators exported by binding into the EXPORT stash under a key computed
# at run time, the way Moneys generates one postfix per currency code.
unit module StashBindDynamicOpMod;

constant %units = { USD => 'dollar', EUR => 'euro' };

my package EXPORT::ALL {
    for %units.keys -> $code {
        OUR::{'&postfix:<' ~ $code ~ '>'} := sub ($n) { "$n $code" };
    }
    for <@@ %%> -> $sym {
        OUR::{"&infix:<$sym>"} := sub ($a, $b) { "$a$sym$b" };
    }
}
