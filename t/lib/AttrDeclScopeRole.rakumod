unit role AttrDeclScopeRole;

# Inside the role body on purpose: this `use` runs at COMPOSITION time, from
# whatever compunit composes the role, not while this file is loading.
use AttrDeclScopeTypes;

has ScopeHandle $!handle;
has ScopeMode $!mode = ScopeOpaque;

method handle() { $!handle }
method mode() { $!mode }
