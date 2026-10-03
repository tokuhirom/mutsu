unit module UseTagColonpair::Defs;
our $LIB is export(:LIB) = 'fontconfig';
constant Flag is export(:types) = int32;
enum Kind is export(:enums) (
    :KindUnknown(-1),
    slip <KindVoid KindInteger>
);
module Inner is export { our sub f { 'inner' } }
