use v6;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;
use Test;
use ToplevelQualifiedNames;

is (try EVAL 'Color::Green') // 'MISSING', 'MISSING',
    'an unexported short enum name is not visible to the importer';
is ToplevelQualifiedNames::Thing.short-enum.Str, 'Green',
    'a method of the declaring module still sees the short name';
is ToplevelQualifiedNames::direct(), 'Red,Green,Blue,Red,Dark',
    'a sub of the declaring module still sees the short name';
is ToplevelQualifiedNames::Color::Green.Str, 'Green',
    'the package-qualified enum member remains visible';

done-testing;
