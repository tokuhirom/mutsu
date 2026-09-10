# The control for t/modules/setting-nested-package-visibility.t: a package
# under a FRESH top-level namespace, reached only transitively. Rakudo does not
# make this one nameable from the outer compunit, and neither may mutsu.
class SettingNestedOwn::Beta {
    method greet(--> Str:D) { 'beta' }
}
