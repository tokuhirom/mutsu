use v6;
use lib 't/lib';
use Test;

use Issue8120ExportedMy;

plan 2;

is ExportedState.new.who, 'exported',
    'an exported my class remains visible after the module load';

class ExportedChild is ExportedState { }
ok ExportedChild.new ~~ ExportedState,
    'an exported my class can be used as an inheritance parent';
