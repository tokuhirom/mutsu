unit module NativeCallHelperUser;

# A module that merely *uses* NativeCall and calls one of its helper routines.
# Rakudo's NativeCall exports `nativecast`; this module does not, and must not
# re-export it to whoever uses this one. mutsu splices the helper into the host
# compunit as a prelude, so getting that wrong is easy — and the consequence is
# not just an extra symbol: the re-exported copy collides with the importer's
# own spliced copy as a hard X::Redeclaration.
#
# Shaped after NativeLibs' `Searcher.find`, which is what made every DBIish
# SQLCipher test file die.

use NativeCall;

our sub cast-through($type, $ptr) is export { nativecast($type, $ptr) }
