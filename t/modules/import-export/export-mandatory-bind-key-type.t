use Test;
use lib 't/lib';
use ExportMandatoryTypes;

plan 3;

class Target { }

is Marker.^name, 'Marker', 'a type copied into EXPORT::MANDATORY is imported';
is Marker[Target], 'marker for Target', 'and keeps its metamethods';
is (1 tag 2), 'tagged', 'the routines copied alongside it still work';
