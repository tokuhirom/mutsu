unit class ModuleScopeSymbolUser;

use ModuleScopeSymbolDefs :MSS;

# The module's OWN file-scope constant, declared here rather than imported.
constant mss-own = 'OWNVAL';

method imported-enum-key() { mssz }
method imported-constant() { mss-const }
method own-constant()      { mss-own }

# A captured local must still beat the module's own file-scope name.
method local-shadows-own() {
    my $mss-own = 'LOCAL';
    my $block = { $mss-own };
    $block()
}
