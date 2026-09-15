use Test;
use lib 't/lib';
use ExportRawRoutine;
use ExportFromUnitRole;

plan 4;

is raw-export(), 42, 'an exported raw routine is imported';
is export-raw(), 43, 'export remains effective after the raw trait';
is capture-export(Int), Int, 'an exported raw routine with a capture parameter is imported';
is role-export(), 44, 'an exported routine from a unit role is imported';
