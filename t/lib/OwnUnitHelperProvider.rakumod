unit module OwnUnitHelperProvider;

sub helper() { 'provider helper' }
sub counted($x) is export { $x + 1 }

class OwnUnitHelperObj is export {
    method via-method() { helper() }
    method via-closure() { my &c = -> { helper() }; c() }
}
