# Exports a custom infix, as Terminal::Widgets::I18N::Translation does.
unit module OpBare::Provider;

sub infix:<¢¿>(Str:D $context, Str:D $string) is export { "$context/$string" }
