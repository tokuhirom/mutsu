//! Interpreter-level smoke tests whose only purpose is to be run **under Miri**
//! (ADR-0013 §4 phase 4, `.github/workflows/miri.yml`).
//!
//! The `gc_ptr` unit tests prove the primitive in isolation; run under Miri they
//! mostly re-prove std's `UnsafeCell` guarantee. What the acceptance gate
//! actually needs is Miri watching a *real* execution path — the VM taking an
//! aliased `&mut` into a shared container node through
//! [`crate::gc::gc_contents_mut`] while other `Gc` handles to that node are
//! live. That is the shape ADR-0013 made sound, and the shape a future refactor
//! could silently un-sound.
//!
//! So each test below runs a tiny Raku program that forces a container mutation
//! to be visible through an alias — the exact case `Gc::make_mut` cannot express
//! (it would COW and sever the alias). Under a normal build these are ordinary,
//! fast assertions; under Miri they are the provenance check.
//!
//! Keep them **small**. Miri is orders of magnitude slower than native, and the
//! job runs them on every `src/gc/**` and `src/value/**` change.
//!
//! # Currently `#[cfg_attr(miri, ignore)]` — blocked on lazy magic vars
//!
//! They do not run under Miri *yet*, so today they are ordinary native tests and
//! the Miri gate covers the primitive only. The blocker is not the container
//! code: `Interpreter::new()` eagerly builds `$*DISTRO` / `$*KERNEL`, which
//! shells out to `uname -r`, `uname -m` and `hostname`, and Miri cannot spawn a
//! process ("unsupported operation: can't call foreign function
//! `posix_spawnattr_init`"). Startup dies before reaching any container code.
//!
//! `todo/tickets/magic-vars-should-be-built-lazily.md` fixes that at the root
//! (build those instances on first access, not at startup — which is also a
//! startup-cost win for every `mutsu` invocation). Drop the `cfg_attr` here when
//! it lands: that is what upgrades the gate from "the primitive is sound" to
//! "the VM's real call sites obey the primitive's contract".

#[cfg(test)]
mod tests {
    use crate::Interpreter;

    fn run(src: &str) -> String {
        let mut interp = Interpreter::new();
        match interp.run(src) {
            Ok(out) => out,
            Err(e) => panic!("program failed: {}", e.message),
        }
    }

    /// `:=` binds a second name to the *same* array node; a push through the
    /// binding must be visible through the original. That write is the aliased
    /// `gc_contents_mut` path (`strong_count > 1`, so `gc_data_mut` takes the
    /// aliased branch rather than `make_mut`).
    #[test]
    #[cfg_attr(miri, ignore)] // see the module header: blocked on lazy magic vars
    fn a_push_through_an_array_binding_is_visible_through_the_original() {
        let out = run("my @a = 1, 2; my @b := @a; @b.push(3); say @a.elems; say @a[2];");
        assert_eq!(out.trim(), "3\n3");
    }

    /// The hash counterpart: an insert through a bound alias reaches the
    /// original node.
    #[test]
    #[cfg_attr(miri, ignore)] // see the module header: blocked on lazy magic vars
    fn an_insert_through_a_hash_binding_is_visible_through_the_original() {
        let out = run("my %h = a => 1; my %g := %h; %g<b> = 2; say %h.elems; say %h<b>;");
        assert_eq!(out.trim(), "2\n2");
    }

    /// Capturing an array by value into a list keeps the same node (Raku list
    /// containers do not copy), so a later push is observable through the
    /// captured copy — the aliasing case the COW branch must not take.
    #[test]
    #[cfg_attr(miri, ignore)] // see the module header: blocked on lazy magic vars
    fn a_captured_array_still_sees_a_later_push() {
        let out = run("my @a = 1; my $l = (0, @a); @a.push(2); say $l[1].elems;");
        assert_eq!(out.trim(), "2");
    }

    /// A cyclic structure exercises the collector's own `&mut` paths (the
    /// fixup sites that carry the `strong_count == 1` uniqueness assertion) in
    /// addition to the mutation path.
    #[test]
    #[cfg_attr(miri, ignore)] // see the module header: blocked on lazy magic vars
    fn a_self_referential_array_builds_and_collects() {
        let out = run("my @a = 1, 2; @a.push(@a); say @a.elems;");
        assert_eq!(out.trim(), "3");
    }
}
