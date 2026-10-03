# A package-less module: its plain `sub ... is export` is `my`-scoped, so it
# reaches another scope only through an import (#11103).
sub plain-own-export() is export { 'own:' ~ plain-own-helper() }
sub plain-own-helper() { 'helper' }
sub plain-own-caller() is export { plain-own-export() }
