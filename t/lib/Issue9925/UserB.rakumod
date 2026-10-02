unit module Issue9925::UserB;
use Issue9925::ThingB;

sub user-b-who() is export { Thing.new.who ~ '/' ~ Thing.^name }
