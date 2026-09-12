# The `Font::AFM` shape: a `unit class` whose parent arrives from a `use` in
# its own (file-scoped) body. See t/oo/class/also-is-body-position.t.
use v6;
unit class AlsoIsBodyUnitKid;
use AlsoIsBodyUseParent;
also is AlsoIsBodyUseParent;

method extra { 'kid+' ~ self.greet }
