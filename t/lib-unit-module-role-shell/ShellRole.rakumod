unit module ShellRole;
use NativeCall;
use ShellStore;
multi trait_mod:<is>(Routine $r, :$probe!) {
    @ShellStore::SEEN.push: $r.signature.params[0].type.^name
}
role R is export { sub g(size_t $x) is probe { }; method z { 1 } }
