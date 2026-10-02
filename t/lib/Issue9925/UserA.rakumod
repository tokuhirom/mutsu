unit module Issue9925::UserA;
use Issue9925::ThingA;

sub user-a-who() is export { Thing.new.who ~ '/' ~ Thing.^name }
