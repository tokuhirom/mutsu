unit module RegexModuleOurVarUser;

# Imports RegexModuleOurVar; the test imports only this module, so the
# interpolated `$NAMED` is not in the test's own scope.
use RegexModuleOurVar;

sub user-recolor(Str $v --> Str) is export { recolor($v) }
sub user-has-named(Str $v --> Bool) is export { has-named($v) }
