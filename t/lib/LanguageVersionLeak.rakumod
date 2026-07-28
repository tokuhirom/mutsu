use v6;
unit module LanguageVersionLeak;

# A pragma-less module: it compiles under the 6.d default, and loading it must
# not drag the importing unit down from 6.e to 6.d.
sub lang-leak-probe() is export { "loaded" }
