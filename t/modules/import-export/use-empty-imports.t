use v6;
use Test;

plan 3;

use lib 't/lib';
use EmptyImportQA Empty;
use EmptyImportQB ();

is EmptyImportQA::LOADED, 'qa-loaded',
    'use Module Empty still loads the module';
is EmptyImportQB::LOADED, 'qb-loaded',
    'use Module () still loads the module';
nok ::('&MAIN').defined,
    'an empty import list does not import or dispatch MAIN';
