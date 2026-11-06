use Test;
use lib 't/lib';
use StashClassThenSubs;
plan 3;

is StashClassThenSubs::.keys.sort.join(','), '',
    'file-scope routines after the class are not members of its stash';

# A `my role` is lexical: never a member of the enclosing class's stash.
class WithRole { my role Hidden { }; my class HiddenC { }; our sub shown() { 1 } }
is WithRole::.keys.sort.join(','), '&shown', 'my role / my class stay out of the stash';

ok lexical-helper() == 3 && plain-helper() == 4, 'the exported routines still import';
