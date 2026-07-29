unit class RequiredDriver;
use RequiredDriver::Native;

has $.parent;

# Both of these resolve `Widget` through the short name this module imported for
# itself, long after whatever frame ran the `require` has returned.
method widget-name(--> Str) { Widget.^name }
method widget-label(--> Str) { Widget.new.label }
method widget-tag(--> Str) { WIDGET-TAG }
