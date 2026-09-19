use v6.*;
use Test;

# User::grent 0.0.4 uses my constant code aliases for setgrent/endgrent.
# A later :FIELDS import must recover those values after an earlier scoped use.
plan 4;

use lib 't/lib';

{
    use ExportedCodeConstant;
    for <&setgrent &endgrent> -> $name {
        ok OUTER::MY::<<$name>>:exists, "default export $name";
    }
}

{
    use ExportedCodeConstant :FIELDS;
    for <&setgrent &endgrent> -> $name {
        ok OUTER::MY::<<$name>>:exists, "tagged re-import $name";
    }
}
