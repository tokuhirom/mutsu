unit module ShellRoleLate:ver<0.1.0>;
use NativeCall;
use ShellStore;
# Run-time code ahead of the role: the role is registered early (a shell),
# for code here that could construct it, but not ahead of the `use`s above.
our $flag = True;
multi trait_mod:<is>(Routine $r, :$probe-late!) {
    @ShellStore::SEEN.push: $r.signature.params[0].type.^name
}
role RL is export { sub g(size_t $x) is probe-late { }; method z { 2 } }
