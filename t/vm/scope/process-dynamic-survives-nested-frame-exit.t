use Test;

plan 6;

# `PROCESS::<$name> := ...` (the `Rakudo::Internals.REGISTER-DYNAMIC` idiom
# several CPAN/zef modules use, e.g. Object::Permission's
# `PROCESS::<$AUTH-USER> := Proxy.new(...)`) installs a process-level default
# that must remain visible and writable from ANY later, unrelated frame --
# even when the install itself ran inside a nested bare block, a `module {
# ... }` body, or a sub call, each of which ends its own frame's env the
# moment it exits. Before this fix, a later `$*name` write from such an
# unrelated frame threw X::Dynamic::NotFound (CheckDynamicVarDeclared saw
# neither an env entry nor a declared-dynamic flag once the installing
# frame was gone), and a later read fell back to nothing at all. See #8682;
# the Proxy case is reduced from Object::Permission (verified against raku).

# --- a bare, unlabelled block, installing a Proxy container ---
{
    my $user;
    PROCESS::<$AUTH-USER> := Proxy.new(
        FETCH => sub ($) { $user },
        STORE => sub ($, $val) { $user = $val },
    );
}
$*AUTH-USER = "hi";
is $*AUTH-USER, "hi",
    'a PROCESS:: Proxy install from a nested bare block is writable/readable after it exits';

# --- a `module { ... }` body, installing a Proxy container ---
module InstallsProcessDynamic1 {
    my $inner;
    PROCESS::<$MOD-DYN> := Proxy.new(
        FETCH => sub ($) { $inner },
        STORE => sub ($, $val) { $inner = $val },
    );
}
$*MOD-DYN = "from-module";
is $*MOD-DYN, "from-module",
    'a PROCESS:: Proxy install from a module body is writable/readable after it exits';

# --- a sub call, installing a plain (non-Proxy) value ---
sub installer() {
    PROCESS::<$SUB-DYN> := 123;
}
installer();
is $*SUB-DYN, 123,
    'a PROCESS:: (non-Proxy) install from inside a sub call is readable after it returns';

$*SUB-DYN = 456;
is $*SUB-DYN, 456,
    'and remains writable/readable with a plain assignment from yet another frame';

sub later_reader() { $*SUB-DYN }
is later_reader(), 456,
    'the updated value is visible from a completely different, unrelated sub frame';

is PROCESS::<$SUB-DYN>, 456,
    'PROCESS::<$x> pseudo-stash read agrees after the cross-frame writes';
