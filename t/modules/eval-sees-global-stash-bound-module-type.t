use lib 't/lib';
use Test;
use GlobalBoundEval;
use GlobalBoundModels;

plan 3;

BEGIN GLOBAL::<GlobalBoundUser> := GlobalBoundUser;

# A module type published with `GLOBAL::<Name> := Name` resolves from code a
# module EVALs, though that module never imported the type.
is run-str('GlobalBoundUser.new.x'), 5, 'a bare type name in a module EVAL resolves through GLOBAL';
is run-str('GlobalBoundUser.^name'), 'GlobalBoundModels::GlobalBoundUser', 'it is the module type';
dies-ok { run-str('NoSuchGlobalBoundType.new') }, 'an unpublished name is still undeclared';
