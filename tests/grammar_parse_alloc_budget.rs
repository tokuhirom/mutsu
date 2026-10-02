//! #10488 / ADR-10488: a grammar parse allocated about twenty times per
//! subrule Match it built.
//!
//! The compiled regex engine assembled a parse's Match tree from per-call
//! capture deltas: every subrule return built a delta with a map and a node
//! vector of its own for the caller's level to copy and free, a separated
//! quantifier and a `~` goal match gave each iteration or side a capture level
//! of its own to fold afterwards, and every call allocated its frame. Capture
//! levels are written in place now, frames live in an arena, and the named
//! axis is a small map in filing order with one node inline.
//!
//! The pin is deterministic rather than timed: a thread-local counting global
//! allocator records heap allocations, and the slope over the document size
//! isolates the per-element cost of a parse from start-up and grammar
//! compilation -- the method of `tests/attribute_source_file_alloc_budget.rs`.
//!
//! Compiled out under the `alloc-stats` feature: that build installs its own
//! counting `#[global_allocator]` in the library, and a crate may link only one.

#![cfg(not(feature = "alloc-stats"))]

use crate::allocs_now;

/// `benchmarks/bench-grammar-parse-big.raku`'s grammar and document shape,
/// parsing a document of `pairs` pairs. The document is built before the
/// count starts, so only the parse is measured.
fn program(pairs: usize) -> String {
    format!(
        r#"grammar JsonLike {{
    token TOP       {{ \s* <value> \s* }}
    rule object     {{ '{{' ~ '}}' <pairlist>     }}
    rule pairlist   {{ <pair> * % \,            }}
    rule pair       {{ <string> ':' <value>     }}
    rule array      {{ '[' ~ ']' <arraylist>    }}
    rule arraylist  {{  <value> * % [ \, ]        }}
    proto token value {{*}};
    token value:sym<number> {{
        '-'?
        [ 0 | <[1..9]> <[0..9]>* ]
        [ \. <[0..9]>+ ]?
        [ <[eE]> [\+|\-]? <[0..9]>+ ]?
    }}
    token value:sym<true>    {{ <sym>    }};
    token value:sym<false>   {{ <sym>    }};
    token value:sym<null>    {{ <sym>    }};
    token value:sym<object>  {{ <object> }};
    token value:sym<array>   {{ <array>  }};
    token value:sym<string>  {{ <string> }}
    token string {{ ('"') ~ \" [ <str> | \\ <str=.str_escape> ]* }}
    token str {{ <-["\\\t\x[0A]]>+ }}
    token str_escape {{ <["\\/bfnrt]> | 'u' <utf16_codepoint>+ % '\u' }}
    token utf16_codepoint {{ <.xdigit>**4 }}
}}
my $inner = '[' ~ (1..4).map({{ "[$_,$_]" }}).join(',') ~ ']';
my $doc = '{{' ~ (1..{pairs}).map({{ "\"k$_\":$inner" }}).join(',') ~ '}}';
die "parse failed" unless JsonLike.parse($doc);
"#
    )
}

/// Allocations made running `src` as a top-level program, after a warm-up
/// run. `ALLOCS` is thread-local, so a fresh thread per measurement starts
/// the count at zero.
fn allocs_for(src: &str) -> u64 {
    const STACK_SIZE: usize = 64 * 1024 * 1024;
    let src = src.to_string();
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || {
            let mut warm = mutsu::Interpreter::new();
            warm.run(&src).expect("program runs");
            drop(warm);

            let before = allocs_now();
            let mut interp = mutsu::Interpreter::new();
            interp.run(&src).expect("program runs");
            allocs_now() - before
        })
        .expect("spawn measurement thread")
        .join()
        .expect("measurement thread")
}

#[test]
fn a_grammar_parse_allocates_a_few_times_per_match_node() {
    const LO: usize = 20;
    const HI: usize = 60;
    let (lo, hi) = (allocs_for(&program(LO)), allocs_for(&program(HI)));
    let per_pair = hi.saturating_sub(lo) as f64 / (HI - LO) as f64;
    eprintln!("grammar parse: {per_pair:.1} allocations per pair ({lo} at {LO}, {hi} at {HI})");
    // A pair is `"kN":[[1,1],[2,2],[3,3],[4,4]]`: 26 subrule Matches, about
    // 32 characters, and the document-building code's own allocations for
    // it. Measured on this program (release): 490.9 allocations per pair
    // before ADR-10488, 76.8 after. The budget trips if a per-call delta
    // (about two allocations per Match) or the per-iteration levels of a
    // separated quantifier or goal match come back.
    let limit = 120.0;
    assert!(
        per_pair <= limit,
        "a grammar parse allocates {per_pair:.1} times per pair (budget {limit}); \
         the compiled engine is building capture deltas or levels per call again -- \
         see ADR-10488 (#10488)"
    );
}
