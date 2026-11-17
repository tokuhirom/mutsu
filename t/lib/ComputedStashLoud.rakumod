# Fixture for t/modules/import-export/use-reads-loaded-unit.t: postfix
# operators exported under keys the module computes while it loads (the
# Moneys pattern), from a module that says when it loads.
unit module ComputedStashLoud;

say "ComputedStashLoud loaded";

my package EXPORT::DEFAULT {
    for <ZZ YY> -> $code {
        OUR::{'&postfix:<' ~ $code ~ '>'} := sub ($n) { "$n $code" };
    }
}

my package EXPORT::extra {
    OUR::{'&postfix:<' ~ 'XX' ~ '>'} := sub ($n) { "$n XX" };
}
