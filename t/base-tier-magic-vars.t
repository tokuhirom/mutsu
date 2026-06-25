use Test;

# Regression pin for docs/vm-dual-store.md Slice 4c: the immutable
# process-constant magic/dynamic variables ($*VM, $*KERNEL, $*EXECUTABLE,
# $*SPEC, $*PID, ...) are hoisted into the shared env base tier instead of
# living in every per-frame env overlay. Reads must still resolve, the
# kebab/underscore alias must work, the values must be visible inside subs,
# blocks, closures and threads, and a `my $*FOO` shadow inside a block must
# not leak out.

plan 14;

# Basic reads resolve through the base tier.
ok $*VM.defined,             '$*VM is defined';
ok $*KERNEL.defined,         '$*KERNEL is defined';
ok $*DISTRO.defined,         '$*DISTRO is defined';
ok $*EXECUTABLE.defined,     '$*EXECUTABLE is defined';
ok $*EXECUTABLE-NAME.defined,'$*EXECUTABLE-NAME is defined';
ok $*SPEC ~~ IO::Spec,       '$*SPEC is an IO::Spec type object';
ok $*PID ~~ Int,             '$*PID is an Int';

# Underscore alias for the kebab-case name.
is $*EXECUTABLE_NAME, $*EXECUTABLE-NAME, '$*EXECUTABLE_NAME aliases $*EXECUTABLE-NAME';

# Visible inside a sub (a per-frame env that no longer carries these entries).
sub reads-vm() { $*VM.^name }
is reads-vm(), $*VM.^name, '$*VM visible inside a sub';

# Visible inside a nested block.
{
    ok $*KERNEL.defined, '$*KERNEL visible inside a block';
}

# Visible inside a closure that does not capture it directly.
my $c = { $*SPEC.^name };
is $c(), $*SPEC.^name, '$*SPEC visible inside a closure';

# A dynamic-scope shadow (a base-tier var promoted into a local overlay) must
# take effect for its scope and not leak to the enclosing scope. Done inside a
# sub to avoid the unrelated read-then-redeclare postdeclaration limitation.
sub shadows-vm() {
    my $*VM = 42;
    $*VM;
}
is shadows-vm(), 42, '$*VM shadowable via my $*VM';
isnt $*VM, 42, '$*VM unaffected outside the shadowing sub';

# Visible across a thread boundary (thread env falls back to the base tier).
my $p = start { $*KERNEL.^name }
is await($p), $*KERNEL.^name, '$*KERNEL visible inside a thread';
