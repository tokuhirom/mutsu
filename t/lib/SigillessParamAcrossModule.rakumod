unit class SigillessParamAcrossModule;

# A method whose parameter is sigilless (`\c`) must observe the value the
# caller actually passed, not a re-read of the caller's variable by name.
method peek(\c) { c }
method peek-and-write(\c) { my $r = c; $r = 'local'; c }
method two(\c, $d) { (c, $d) }

sub peek-sub(\c) is export { c }
