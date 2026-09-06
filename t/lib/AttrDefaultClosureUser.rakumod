unit class AttrDefaultClosureUser;
use AttrDefaultClosureScope;

has $.inner;

# Constructing from inside ANOTHER module's method is what used to stamp this
# class's name onto the other class's attribute-default closures.
submethod BUILD() { $!inner = AttrDefaultClosureScope.new }

method attr($x) { $!inner.via-attr($x) }
method table($k, $x) { $!inner.via-table($k, $x) }
method nested($x) { $!inner.via-nested($x) }
method local($x) { $!inner.via-local($x) }
method direct($x) { $!inner.direct-call($x) }
