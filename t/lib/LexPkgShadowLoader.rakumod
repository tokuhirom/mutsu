# Loads LexPkgShadowGlobal without exporting it: its importer reaches
# `LexPkgShadowName` only transitively (#18409).
use LexPkgShadowGlobal;
unit class LexPkgShadowLoader;
method load(Str $code) { LexPkgShadowName.load($code) }
