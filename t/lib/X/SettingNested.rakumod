# Declares packages under two namespaces the SETTING itself provides (`X`,
# `IO`). Loaded only TRANSITIVELY, through `SettingNestedHost`, so a script
# that `use`s the host still sees both -- see
# t/modules/setting-nested-package-visibility.t.
class X::SettingNested::Alpha {
    also is Exception;
    method message(--> Str:D) { 'alpha' }
}

class IO::SettingNestedProbe {
    method greet(--> Str:D) { 'io-probe' }
}
