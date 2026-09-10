use X::SettingNested;
use SettingNestedOwn;

unit class SettingNestedHost;

method alpha-message(--> Str:D) { X::SettingNested::Alpha.new.message }
method own-greet(--> Str:D) { SettingNestedOwn::Beta.greet }
