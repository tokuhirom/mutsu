use Test;

# Regression pin for issue #8652: a LATER, unrelated `my $*OUT = ...`
# redeclaration anywhere else in the same compiled unit (mainline chunk) used
# to desync an EARLIER `$*OUT` reassignment for `say`/`print`, even though the
# later declaration's block had not run yet.
#
# Root cause: `code.locals` is a single flat table for the whole compiled
# unit, so the later, block-scoped `my $*OUT = ...` allocates a local slot for
# the bare name `*OUT` even before its own declaration executes. At frame
# entry, that slot is seeded from whatever `env["*OUT"]` held at the time (the
# real stdout handle). `sync_env_from_locals_declared` /
# `sync_env_from_locals_needed` (run before Say/Print and before some calls)
# then republished EVERY local whose bare name already exists in `env` --
# including that still-undeclared slot -- clobbering the correct value an
# earlier, slot-less top-level `my $*OUT = ...` (which compiles straight to
# `SetGlobal`, sharing the bare name with no local slot of its own) had just
# written. Fixed by only publishing a dynamic (`*`-twigil) local's slot once
# ITS OWN declaration has actually run in a currently open block scope
# (`Interpreter::dynamic_local_slot_is_live`).

plan 2;

class Sink {
    method print(*@_ --> True) { }
}

{
    my $out = my $*OUT = Sink.new;
    my $seen1 = $*OUT.^name;
    say "leaked?";
    my $seen2 = $*OUT.^name;
    say "x";
    is $seen1, 'Sink', '$*OUT resolves to the redeclaration right after assignment';
    is $seen2, 'Sink', '$*OUT is still the redeclaration after an intervening say';
    {
        my $*OUT = Sink.new;
    }
    sub noop() { }
    noop();
}
