unit module IsDefaultMultiModule;
my proto sub to-emoji(|) is export {*}
my multi sub to-emoji(Str:D $text) is default { "one" }
my multi sub to-emoji(Str:D $text, %additional) { "two" }
my multi sub to-emoji(Str:D $text, *%_) { to-emoji($text, %_) }
