# Imports the `:Time` tag for itself; an importer of this module must not see `s`.
unit module TaggedEnumQuote::Mid;

use TaggedEnumQuote::Units :Time;

our sub mid { ~s }
