unit class RequiredDriver::Second;
use RequiredDriver::Native;

# A SECOND importer of the same module. By the time this one loads, `WIDGET-TAG`
# and `Widget` are already in `env` from the first importer, so the env diff
# taken around this module's body sees nothing new -- the import itself has to be
# what attributes them to this module's scope.
has $.parent;

method tag(--> Str) { WIDGET-TAG }
method widget-name(--> Str) { Widget.^name }
