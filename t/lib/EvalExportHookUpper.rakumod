# Its only export is made by `sub EXPORT` at import time, so nothing static
# can tell an EVAL'd `use` which names it brings in (#13441).
sub EXPORT { %( 'Thing' => 42, '&shout' => sub ($s) { $s.uc } ) }
