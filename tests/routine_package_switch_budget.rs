//! #8686 Phase 1: calling a routine declared in a non-`GLOBAL` package must not
//! re-intern that package's name once per call.
//!
//! Every call into a module's sub brackets its body with a `current_package`
//! switch, so that an unqualified name in the body resolves against the
//! declaring package. Both halves of that bracket went through the by-name
//! `set_current_package`, which calls `Symbol::intern` — so a light call into
//! `JSON::Fast`'s own helper re-hashed the string `"JSON::Fast"` on the way in
//! and again on the way out, for a package whose `Symbol` the callee's
//! `CompiledFunction::package_sym` had already interned once at registration.
//! The saved package is now carried as that `Symbol`, and neither the switch
//! nor its restore writes anything while the package has not actually moved —
//! which is the shape of every call to a sibling declared in the same package.
//!
//! Method and rationale are `tests/named_call_intern_budget.rs`'s; see that
//! file for why this is a deterministic intern count rather than a timing, and
//! for why a `debug_assert!` that interns silently inflates every number here.
//! (The `debug_assert_eq!` this change added to
//! `enter_routine_package_outlined` deliberately uses `Symbol::as_str`, the
//! resolve direction, which interns nothing.)

/// Interns performed while running `src`, measured after a warm-up run so that
/// one-time interning (parsing, compiling, first pass through each code path)
/// is not counted.
fn interns_for(src: &str) -> u64 {
    let mut warm = mutsu::Interpreter::new();
    warm.run(src).expect("program runs");
    drop(warm);

    let before = mutsu::symbol::intern_calls();
    let mut interp = mutsu::Interpreter::new();
    interp.run(src).expect("program runs");
    mutsu::symbol::intern_calls() - before
}

/// Interns attributable to one extra iteration of a loop, isolated from the
/// program's fixed startup cost by running the same program at two lengths.
fn interns_per_iteration(program: impl Fn(usize) -> String) -> f64 {
    const LOW: usize = 100;
    const HIGH: usize = 1100;
    let lo = interns_for(&program(LOW));
    let hi = interns_for(&program(HIGH));
    assert!(
        hi >= lo,
        "interning cannot shrink with more iterations: {lo} -> {hi}"
    );
    (hi - lo) as f64 / (HIGH - LOW) as f64
}

/// The same loop with the call replaced by plain arithmetic. Subtracting it
/// leaves the call's own interning, so a budget here is not inflated by
/// whatever the surrounding loop costs.
fn interns_per_call(preamble: &str, body: &str) -> f64 {
    let control =
        interns_per_iteration(|n| format!("my $hits = 0;\nfor ^{n} {{ $hits = $hits + 1 }}\n"));
    let measured =
        interns_per_iteration(|n| format!("{preamble}\nmy $hits = 0;\nfor ^{n} {{ {body} }}\n"));
    measured - control
}

/// A plain-scalar signature with no traits and no native types is light-call
/// eligible, so this reaches `enter_routine_package` /
/// `leave_routine_package` — the pair that went through the interning by-name
/// `set_current_package` twice per call.
#[test]
fn a_call_into_a_module_sub_does_not_intern_its_package() {
    let per_call = interns_per_call(
        "module M { our sub helper($a) { $a + 1 } }",
        r#"$hits = M::helper($hits);"#,
    );
    eprintln!("module sub call: {per_call:.3} interns per call");
    // Measured 3.0 before this change and 1.0 after: the package switch was
    // exactly 2.0 of it (one intern of `"M"` entering, one of `"GLOBAL"`
    // restoring). The remaining 1.0 was `push_routine_with_location`
    // re-splitting and re-interning this qualified callsite's short name
    // (`"helper"`) on every call
    // ([#8776](https://github.com/tokuhirom/mutsu/issues/8776)); since it goes
    // through the memoized `qualified::unqualified_part` it measures 0.0.
    // The budget stays at 0.5 rather than 0 so a stray one-off intern does not
    // fail it, while any per-call intern (>= 1.0) still does.
    let limit = 0.5;
    assert!(
        per_call <= limit,
        "a call into a module sub re-interns its declaring package per call \
         ({per_call:.3} interns/call, budget {limit}); see #8686"
    );
}

/// A deeply-qualified package name is the case the intern actually cost real
/// work for: `Symbol::intern` hashes and compares the whole key even on a hit,
/// so the per-call cost used to grow with the length of the module's name. The
/// budget is the same as above, which is the point — it must not scale.
#[test]
fn the_package_intern_budget_does_not_grow_with_the_package_name() {
    let short = interns_per_call(
        "module M { our sub helper($a) { $a + 1 } }",
        r#"$hits = M::helper($hits);"#,
    );
    let long = interns_per_call(
        "module Deeply::Nested::Package::With::A::Long::Name \
         { our sub helper($a) { $a + 1 } }",
        r#"$hits = Deeply::Nested::Package::With::A::Long::Name::helper($hits);"#,
    );
    eprintln!("short package: {short:.3}, long package: {long:.3} interns per call");
    // Both measure 1.0 after the change; both measured 3.0 before it, which is
    // why the equality rather than the absolute number is the assertion here.
    assert!(
        long <= short + 0.5,
        "the per-call intern budget grows with the declaring package's name \
         ({short:.3} -> {long:.3} interns/call), so the package is being \
         re-hashed per call; see #8686"
    );
}
