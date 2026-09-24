use v6.d;
use Test;
use lib 't/lib';

plan 4;

use ImportedExportedProtoWrapper;
use ImportedExportedProtoProvider;

is imported-exported-calendar(1..4, format => 'html'), 'wrapper',
    'a local multi extending an imported exported proto is callable';
is imported-exported-calendar-year(format => 'html'), 'wrapper-year',
    'a zero-positional-argument wrapper candidate is callable';
is &imported-exported-calendar.candidates.elems, 2,
    'the imported and local candidates remain one family';
is &imported-exported-calendar-year.candidates.elems, 2,
    'the year family retains both candidates';
