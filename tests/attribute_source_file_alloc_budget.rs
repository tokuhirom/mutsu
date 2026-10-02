//! #10090: constructing an object allocated a `String` per attribute on
//! every construction, twice.
//!
//! PR #10060 gave `ClassAttributeDef` the `has`-declaration site of its
//! auto-generated accessor (`source_line`/`source_file`), with the file as an
//! owned `Option<String>`. The bless path clones the class's attribute list
//! (`collect_class_attributes`, walked twice per construction), so every
//! attribute of every constructed object paid two extra heap allocations just
//! to carry a path nobody reads during construction: +44 allocations per
//! `Dist.new` in `benchmarks/bench-ctor.raku` (22 attributes), a +16.6% step in
//! its `bench-det` allocation series. The field is an interned `Symbol` now,
//! which clones for free.
//!
//! The pin is deterministic rather than timed: a thread-local counting global
//! allocator records heap allocations, and a slope over the attribute count
//! isolates the per-attribute cost of one construction from everything that
//! does not scale with it -- the same method as
//! `tests/source_file_sym_fallback_alloc_budget.rs`.
//!
//! Compiled out under the `alloc-stats` feature: that build installs its own
//! counting `#[global_allocator]` in the library, and a crate may link only one.

#![cfg(not(feature = "alloc-stats"))]

use crate::allocs_now;

/// Allocations made running `src` as a top-level program, with `program_path`
/// set the way the CLI always sets it (so the declaration site has a file to
/// record), measured after a warm-up run. `ALLOCS` is thread-local, so a
/// fresh thread per measurement starts the count at zero.
fn allocs_for(src: &str) -> u64 {
    const STACK_SIZE: usize = 64 * 1024 * 1024;
    let src = src.to_string();
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || {
            let mut warm = mutsu::Interpreter::new();
            warm.set_program_path("script.raku");
            warm.run(&src).expect("program runs");
            drop(warm);

            let before = allocs_now();
            let mut interp = mutsu::Interpreter::new();
            interp.set_program_path("script.raku");
            interp.run(&src).expect("program runs");
            allocs_now() - before
        })
        .expect("spawn measurement thread")
        .join()
        .expect("measurement thread")
}

/// A program constructing `iters` objects of a class with `attrs` public
/// scalar attributes, none of them initialised by the caller. The class has
/// `benchmarks/bench-ctor.raku`'s shape -- a `new` delegating to `bless` and
/// a `TWEAK` -- which is the construction path that walks the attribute list;
/// a plain default `.new` takes a plan-backed fast path that never clones it.
fn program(attrs: usize, iters: usize) -> String {
    let decls: String = (0..attrs).map(|i| format!("has $.a{i}; ")).collect();
    format!(
        "class C {{ {decls}method new(*%_) {{ self.bless(|%_) }}; submethod TWEAK() {{ }} }}\n\
         my $n = 0;\nfor ^{iters} {{ $n++ if C.new.defined }}\nsay $n;"
    )
}

/// Allocations per attribute per construction: the slope over the attribute
/// count of the per-construction slope over the iteration count. The inner
/// slope cancels class registration and program startup (which also scale
/// with the attribute count, but only once); the outer one cancels the
/// per-construction cost that does not depend on the attribute count.
fn allocs_per_attribute_per_construction() -> f64 {
    const ITERS_LO: usize = 50;
    const ITERS_HI: usize = 250;
    const ATTRS_LO: usize = 2;
    const ATTRS_HI: usize = 22;
    let per_construction = |attrs: usize| {
        let lo = allocs_for(&program(attrs, ITERS_LO));
        let hi = allocs_for(&program(attrs, ITERS_HI));
        hi.saturating_sub(lo) as f64 / (ITERS_HI - ITERS_LO) as f64
    };
    let narrow = per_construction(ATTRS_LO);
    let wide = per_construction(ATTRS_HI);
    eprintln!("per construction: {narrow:.3} ({ATTRS_LO} attrs), {wide:.3} ({ATTRS_HI} attrs)");
    (wide - narrow) / (ATTRS_HI - ATTRS_LO) as f64
}

#[test]
fn construction_does_not_clone_a_source_file_string_per_attribute() {
    let per_attr = allocs_per_attribute_per_construction();
    eprintln!("construction: {per_attr:.3} allocations per attribute");
    // Measured on this test's own program: 2.149 allocations per attribute
    // per construction before the fix, 1.151 after -- the clone of the
    // `source_file` `String` (#10090). The budget sits halfway, so it trips if
    // that clone comes back and leaves the remaining per-attribute cost (the
    // attribute's name clone) alone.
    let limit = 1.65;
    assert!(
        per_attr <= limit,
        "constructing an object allocates {per_attr:.3} times per attribute \
         (budget {limit}); a per-attribute String in ClassAttributeDef is being \
         cloned on the bless path again -- see #10090"
    );
}
