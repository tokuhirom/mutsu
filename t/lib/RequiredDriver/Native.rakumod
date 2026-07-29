unit module RequiredDriver::Native;

# A type the driver below imports for its OWN lexical scope. It is exported, so
# `use RequiredDriver::Native` puts the short name `Widget` in the importing
# module's scope -- which is exactly the alias a `require` run inside a method
# frame used to lose (see t/require-in-method-keeps-module-type-alias.t).
class Widget is export {
    method label(--> Str) { 'widget' }
}

constant WIDGET-TAG is export = 'tagged';
