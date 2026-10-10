unit module OwnUnitHelperConsumer;
use OwnUnitHelperProvider;

sub helper() { 'consumer helper' }

sub through-consumer() is export {
    OwnUnitHelperObj.new.via-method ~ ' / ' ~ OwnUnitHelperObj.new.via-closure
}
sub own-helper() is export { helper() }
