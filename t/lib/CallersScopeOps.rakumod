unit module CallersScopeOps;

role Node is export {}
class Leaf does Node is export {}

sub __scope_marker is export { True }
multi prefix:<!>(Node $n) is export { 'node' }
multi prefix:<!>(Leaf $n) is export { 'leaf' }
