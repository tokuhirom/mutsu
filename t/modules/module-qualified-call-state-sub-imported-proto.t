use lib 't/lib';
use Test;

# Found via JavaScript::Google::Charts: a module sub that declares `state`
# (here implicitly, through "{$0}") and calls an imported `proto` routine died
# with "Routine does not have any candidates" when it was called qualified
# (`Mod::sub(...)`): the shared body captured for state-declaring module subs
# was compiled without the module's lexical import sets.
plan 2;

use QualStateSubProtoUser;

is gen([1]), "multi:aa", "bare call";
is QualStateSubProtoUser::gen([1]), "multi:aa", "qualified call";
