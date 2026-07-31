unit class NativeCallHelperPeer;

# The second half of the collision: another compunit that uses NativeCall and
# calls the same helper, so it receives its own spliced copy of the routine. If
# `NativeCallHelperUser` re-exported `nativecast` into the importer's scope,
# loading this one after it died with "Redeclaration of routine 'nativecast'".

use NativeCall;

method cast($type, $ptr) { nativecast($type, $ptr) }
