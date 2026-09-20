use ImportedCoercionType;

# Uses the IMPORTED short name `Wrapped` as a coercion target inside routines
# declared here -- a sub, a class method and a role method.
# See t/modules/import-export/imported-type-name-in-coercion.t.

sub from-sub(--> Wrapped()) is export { 'sub' }

class ICClass is export {
    method from-method(--> Wrapped()) { 'method' }
}

role ICRole {
    method from-role-method(--> Wrapped()) { 'role' }
}
sub make-ic-role() is export { ICRole.new }

sub name-still-resolves() is export { Wrapped.^name }
