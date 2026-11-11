# Fixture for t/modules/import-export/begin-merge-generated-export.t (#15371): a
# BEGIN phaser installs the unit's `&EXPORT` through the RakuAST resolver, as
# CompUnit::Util's `steal-export-sub` does, and finds the unit's own lexical
# `EXPORTHOW` package through `$*R.resolve-lexical-constant`.
use experimental :rakuast;

my package EXPORTHOW { package DECLARE { constant gadget = Metamodel::ClassHOW; } }

BEGIN {
    my $decl := $*R.resolve-lexical-constant('EXPORTHOW', :current-scope-only);
    my $found = $decl.compile-time-value.WHO<DECLARE>.WHO<gadget>.^name;
    my &hook = sub (*@) {
        Map.new('&generated' => sub { 'from the generated EXPORT' },
            '&found-exporthow' => sub { $found })
    };
    $*CU.merge-generated-lexical-declaration:
        RakuAST::Declaration::Import.new(:lexical-name('&EXPORT'), :compile-time-value(&hook)),
        :resolver($*R), :force;
}
