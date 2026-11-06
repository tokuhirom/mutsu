unit module BodiedProtoUnitOtherName;

our proto bodied-proto-sel(\thing, *@positions) is export {
    die "need a position" unless @positions;
    "item={thing} at {@positions.join(',')}"
}
