unit module NativeCallNestedUser;

# A module that loads NativeCall for its own `is native` routines, before its
# importer does.
use NativeCall;

sub nested-getpid(--> int32) is native('c', v6) is symbol('getpid') { * }

our sub nested-pid is export { nested-getpid() }

multi trait_mod:<is>(Routine $r, :$nested-tag!) is export {
    $r.wrap(-> |c { "nested<" ~ callsame() ~ ">" })
}
