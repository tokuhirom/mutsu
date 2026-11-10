use lib 't/lib';
use Test;

# A dynamic variable a module exports with `our $*X is export` is read through
# the dynamic scope: the importer's `$*X := ...` is what the module's own code
# sees, also when another module loaded the exporter first (Qwiratry's
# `$*TRANSFORM-ROOT`).

plan 4;

use OurDynExportMid;
use OurDynExport;

is OurDynReader.new.get, 'unset', 'unbound: the declared default';
$*TROOT := 'R';
is OurDynReader.new.get, 'R', "the module's class reads the importer's binding";
is our-dyn-mid-get(), 'R', "so does a routine of a module that loaded it first";
is $*TROOT, 'R', 'the importer reads its own binding';
