//! ADR-0065 S0: the long-lived-process viability probe (decision D8).
//!
//! mutsu has always been a one-shot process: parse once, run, exit. A language
//! server inverts that — one process parses many documents, and re-parses each
//! of them thousands of times. Before any server skeleton is written, this file
//! pins the properties that whole plan rests on.
//!
//! **What is deliberately NOT asserted: byte-identical ASTs.** The parser mints
//! process-unique ids for constructs whose identity outlives the parse — a
//! `my class`'s `decl_id` is its key in the global type registry (ADR-0047 D1),
//! and `__ANON_CLASS_N__` / `__ANON_ROLE_N__` are registry names. Those counters
//! are monotonic *on purpose*: resetting them per parse would let two different
//! declaration sites in two different compilation units collide in a global
//! table. So the probe compares ASTs with those ids normalized, and any
//! remaining difference is genuine residual parser state.
//!
//! Iteration count is `MUTSU_S0_ITERATIONS` (default: enough to expose a
//! per-parse leak, small enough for `cargo test` on a debug build). Run
//! `cargo test --test long_lived_parse -- --nocapture` to see the measurements.

use std::sync::{Mutex, MutexGuard};
use std::time::Instant;

/// `cargo test` runs the tests in one binary concurrently by default, and two
/// of these measure a *process-global* quantity (the symbol table's size, the
/// process's resident memory). A sibling test parsing at the same time would
/// silently inflate both. Serialize the whole file.
static SERIALIZE: Mutex<()> = Mutex::new(());

fn exclusive() -> MutexGuard<'static, ()> {
    // A panicking test poisons the lock; the later tests are still meaningful.
    SERIALIZE.lock().unwrap_or_else(|e| e.into_inner())
}

/// A document shaped like something a language-server client would hold open:
/// a module with classes, roles, a grammar, and the desugaring-heavy constructs
/// (`state`, `with`, `given`/`when`, `gather`/`take`, anonymous classes,
/// chained index assignment) that mint generated names during parsing.
const DOCUMENT: &str = r#"
unit module Probe::Doc;

role Greeter {
    method greet($name) { "hello, $name" }
}

class Counter does Greeter {
    has Int $.count is rw = 0;
    has %.seen;

    method bump(--> Int) {
        state $calls = 0;
        $calls++;
        $!count++;
        %!seen{$!count} = $calls;
        return $!count;
    }

    method describe() {
        with $!count -> $c {
            given $c {
                when 0  { "empty" }
                when 1  { "single" }
                default { "many ($c)" }
            }
        }
        else {
            "undefined"
        }
    }
}

grammar Probe::Grammar {
    token TOP   { <ident>+ % \s+ }
    token ident { <[ A..Z a..z _ ]> <[ A..Z a..z 0..9 _ ]>* }
}

sub collect(@items) is export {
    gather for @items -> $item {
        take $item.uc if $item ~~ Str;
    }
}

sub tally(%data) is export {
    my @rows;
    for %data.sort(*.key) -> $pair {
        @rows[$pair.value][0] = $pair.key;
    }
    my $anon = class { method label() { "anon" } }.new;
    @rows.map({ $_ // $anon.label }).join(",");
}

my $c = Counter.new;
$c.bump for ^3;
say $c.describe;
say collect(<a b c>);
"#;

/// Prefixes whose trailing digit run is a *process-unique* id, not content.
/// Everything else in the AST dump must be reproducible.
const GENERATED_ID_PREFIXES: &[&str] = &[
    "__ANON_CLASS_",
    "__ANON_ROLE_",
    "__ANON_SUBSET_",
    "__with_tmp_",
    "__if_bind_tmp_",
    "__take_value_",
    "__anon_state_",
    "__anon_array_",
    "__tmp_index_",
    "__supply_emitter_",
    "decl_id: ",
    // Symbol ids shift as the interner grows, so the numeric half of a
    // `Symbol(19: "name")` dump is incidental; the name it prints is not.
    "Symbol(",
];

/// Replace the digit run following each generated-id prefix with `N`, so two
/// dumps of the same document compare equal iff they differ only in ids the
/// parser is *required* to make unique per site.
fn normalize_generated_ids(dump: &str) -> String {
    let mut out = String::with_capacity(dump.len());
    let mut rest = dump;
    'outer: while !rest.is_empty() {
        for prefix in GENERATED_ID_PREFIXES {
            if let Some(after) = rest.strip_prefix(prefix) {
                let digits =
                    after.len() - after.trim_start_matches(|c: char| c.is_ascii_digit()).len();
                if digits > 0 {
                    out.push_str(prefix);
                    out.push('N');
                    rest = &after[digits..];
                    continue 'outer;
                }
            }
        }
        let ch = rest.chars().next().expect("non-empty");
        out.push(ch);
        rest = &rest[ch.len_utf8()..];
    }
    out
}

/// Resident set size in KiB, or `None` where `/proc` is unavailable.
fn rss_kib() -> Option<usize> {
    let statm = std::fs::read_to_string("/proc/self/statm").ok()?;
    let resident_pages: usize = statm.split_whitespace().nth(1)?.parse().ok()?;
    Some(resident_pages * 4)
}

fn iterations() -> usize {
    std::env::var("MUTSU_S0_ITERATIONS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(200)
}

fn report_divergence(what: &str, baseline: &str, other: &str) -> String {
    let mut report = String::new();
    for (a, b) in baseline.lines().zip(other.lines()).filter(|(a, b)| a != b) {
        report.push_str(&format!("  first: {}\n  now  : {}\n", a.trim(), b.trim()));
        if report.len() > 2000 {
            report.push_str("  ...\n");
            break;
        }
    }
    if report.is_empty() {
        report.push_str("  (line counts differ)\n");
    }
    format!("{what}\n{report}")
}

/// The core gate: a resident process that re-analyses an unchanged document
/// must keep producing the same analysis, and must not grow without bound
/// while doing so.
#[test]
fn repeated_parse_of_an_unchanged_document_is_stable() {
    let _guard = exclusive();
    let n = iterations();

    // Warm up: the first parses populate one-time global state (the symbol
    // table's fixed names, `LazyLock` well-known symbols, the pragma preseeds).
    // The steady state is what a resident process actually lives in, so the
    // measurement starts after it is reached.
    let baseline = normalize_generated_ids(&mutsu::dump_ast(DOCUMENT).expect("document parses"));
    for _ in 0..2 {
        mutsu::dump_ast(DOCUMENT).expect("document parses");
    }

    let symbols_before = mutsu::symbol::interned_count();
    let rss_before = rss_kib();
    let started = Instant::now();

    let mut first_divergence: Option<(usize, String)> = None;
    for i in 0..n {
        let ast = normalize_generated_ids(&mutsu::dump_ast(DOCUMENT).expect("document parses"));
        if ast != baseline && first_divergence.is_none() {
            first_divergence = Some((i, ast));
        }
    }

    let elapsed = started.elapsed();
    let symbols_after = mutsu::symbol::interned_count();
    let rss_after = rss_kib();
    let symbol_growth = symbols_after - symbols_before;

    println!(
        "--- ADR-0065 S0 probe: {n} parses of a {}-byte document ---",
        DOCUMENT.len()
    );
    println!(
        "  wall clock      : {elapsed:?} total, {:?} per parse",
        elapsed / n as u32
    );
    println!(
        "  interned symbols: {symbols_before} -> {symbols_after} (+{symbol_growth}, {:.2}/parse)",
        symbol_growth as f64 / n as f64
    );
    match (rss_before, rss_after) {
        (Some(before), Some(after)) => println!(
            "  resident memory : {before} KiB -> {after} KiB ({:+} KiB, {:.3} KiB/parse)",
            after as isize - before as isize,
            (after as f64 - before as f64) / n as f64,
        ),
        _ => println!("  resident memory : unavailable (no /proc)"),
    }

    if let Some((i, ast)) = first_divergence {
        panic!(
            "{}",
            report_divergence(
                &format!(
                    "re-parsing an unchanged document produced a different AST at iteration {i}, \
                     beyond the process-unique ids this probe normalizes away. That is residual \
                     parser state: a resident server would report analyses that change without \
                     the document changing."
                ),
                &baseline,
                &ast,
            )
        );
    }

    // Interning: the document declares one anonymous class, whose registry name
    // (`__ANON_CLASS_N__`) is minted fresh per parse and interned — and interned
    // strings are leaked for the process lifetime by design (`src/symbol.rs`).
    // One name per anonymous declaration per parse is therefore the *expected*
    // cost, and this bound catches a new source of per-parse interning rather
    // than that known one. See `docs/adr/0065-...` S0 findings.
    let anon_declarations_in_document = 1;
    let allowed = anon_declarations_in_document * n;
    assert!(
        symbol_growth <= allowed,
        "re-parsing an unchanged document interned {symbol_growth} new names over {n} parses \
         ({:.2}/parse), above the {:.2}/parse expected from its {anon_declarations_in_document} \
         anonymous declaration(s). Interned strings are leaked for the process lifetime, so a new \
         per-parse intern is an unbounded leak in a resident server.",
        symbol_growth as f64 / n as f64,
        anon_declarations_in_document as f64,
    );

    // Resident memory is noisier than the symbol table (allocator arenas, memo
    // tables that hold a bounded working set), so this bound is a regression
    // tripwire: a genuine per-parse leak grows proportionally to `n`.
    if let (Some(before), Some(after)) = (rss_before, rss_after) {
        let growth = after.saturating_sub(before);
        assert!(
            growth < 32 * 1024,
            "resident memory grew {growth} KiB over {n} parses of an unchanged document"
        );
    }
}

/// The tighter half of the same gate. `DOCUMENT` declares an anonymous class,
/// whose registry name is minted and interned fresh per parse — an expected,
/// documented cost. A document with no anonymous declaration has no such
/// excuse, so its interning must reach exactly zero growth: anything else is an
/// unbounded leak in a resident server, since interned strings are never freed.
#[test]
fn a_document_without_anonymous_declarations_interns_nothing_on_reparse() {
    let _guard = exclusive();
    const PLAIN: &str = r#"
unit module Probe::Plain;

class Point {
    has Int $.x = 0;
    has Int $.y = 0;
    method norm(--> Int) { $!x * $!x + $!y * $!y }
    method shifted(Int $dx, Int $dy) { Point.new(x => $!x + $dx, y => $!y + $dy) }
}

sub distances(@points) is export {
    @points.map({ .norm }).sort;
}

my @pts = Point.new(x => 1, y => 2), Point.new(x => 3, y => 4);
for distances(@pts) -> $d {
    say $d if $d > 1;
}
"#;

    let n = iterations();
    let baseline = normalize_generated_ids(&mutsu::dump_ast(PLAIN).expect("document parses"));
    for _ in 0..2 {
        mutsu::dump_ast(PLAIN).expect("document parses");
    }

    let symbols_before = mutsu::symbol::interned_count();
    let rss_before = rss_kib();
    for _ in 0..n {
        let ast = normalize_generated_ids(&mutsu::dump_ast(PLAIN).expect("document parses"));
        assert_eq!(baseline, ast, "re-parsing an unchanged document diverged");
    }
    let symbol_growth = mutsu::symbol::interned_count() - symbols_before;
    let rss_after = rss_kib();

    println!("--- ADR-0065 S0 probe: {n} parses, no anonymous declarations ---");
    println!(
        "  interned symbols: +{symbol_growth} ({:.3}/parse)",
        symbol_growth as f64 / n as f64
    );
    if let (Some(before), Some(after)) = (rss_before, rss_after) {
        println!(
            "  resident memory : {:+} KiB ({:.3} KiB/parse)",
            after as isize - before as isize,
            (after as f64 - before as f64) / n as f64,
        );
    }

    assert_eq!(
        symbol_growth, 0,
        "re-parsing a document with no anonymous declarations still interned {symbol_growth}          new names over {n} parses; interned strings are leaked for the process lifetime"
    );
}

/// The hazard a one-shot process can never expose: parsing document A must not
/// change how document B parses afterwards. The parser keeps user-defined
/// operators, declared lexicals, the language version and slang modes in
/// thread-local state; any of it surviving a compilation unit would make a
/// resident server's diagnostics depend on which files were opened first.
///
/// There is no pristine process to compare against inside one test, so the
/// comparison is B-before-A against B-after-A: any difference is residue.
#[test]
fn one_document_leaves_no_parser_residue_for_the_next() {
    let _guard = exclusive();
    // A declares everything the parser tracks lexically: a custom operator, a
    // language version pragma, lexical names, a package, a constant, a slang
    // mode change, and a `sub` whose signature the parser preseeds.
    const DOC_A: &str = r#"
use v6.e.PREVIEW;
sub infix:<qqq>($a, $b) { $a + $b }
sub prefix:<^^^>($a) { -$a }
constant ONLY-IN-A = 42;
my $lexical-of-a = 1;
my class LexClass { method m() { 1 } }
say 1 qqq 2;
"#;

    // B uses names A declared. Whether they parse as operators / known names is
    // exactly what must not depend on A having been parsed.
    const DOC_B: &str = r#"
my $x = 1;
say $x;
sub uses-them($a, $b) { $a + $b }
say uses-them(1, 2);
say ONLY-IN-A ~~ Any;
say $lexical-of-a ~~ Any;
"#;

    let describe = |src: &str| match mutsu::dump_ast(src) {
        Ok(ast) => normalize_generated_ids(&ast),
        Err(e) => format!("PARSE ERROR: {}", e.message),
    };

    let b_first = describe(DOC_B);
    describe(DOC_A);
    let b_after_a = describe(DOC_B);
    assert_eq!(
        b_first,
        b_after_a,
        "{}",
        report_divergence(
            "parsing one document changed how the NEXT document parses. The parser's \
             thread-local lexical state (user-defined operators, declared names, language \
             version, slang modes) is leaking across compilation units, which in a resident \
             server means a file's diagnostics depend on which files were opened before it.",
            &b_first,
            &b_after_a,
        )
    );

    // And the reverse order, so the check is not satisfied by B simply failing
    // identically both times.
    let a_first = describe(DOC_A);
    describe(DOC_B);
    let a_after_b = describe(DOC_A);
    assert_eq!(
        normalize_generated_ids(&a_first),
        normalize_generated_ids(&a_after_b),
        "parsing document B changed how document A parses afterwards"
    );

    // A must actually have parsed — otherwise this test proves nothing.
    assert!(
        !a_first.starts_with("PARSE ERROR"),
        "the residue probe's document A no longer parses: {a_first}"
    );
}

/// D8 asks specifically about `ORIGINAL_SOURCE`, the thread-local `(raw pointer,
/// length)` pair the parser uses for `$?LINE`. A resident server parses many
/// short-lived buffers, so a stale pointer left behind by one parse would make
/// the *next* parse's line numbers wrong — and line numbers are what every
/// diagnostic is anchored to (D6).
#[test]
fn line_numbers_survive_repeated_parses_of_differently_sized_buffers() {
    let _guard = exclusive();
    let short = "say 1;\nsay 2;\n";
    // A buffer with a nested sub-parse (heredoc + EVAL) on a different, longer
    // allocation, which is what historically left the pointer dangling.
    let with_nested =
        "my $h = q:to/END/;\nheredoc body\nEND\nsay $h;\nsay EVAL('1 + 1');\nsay 3;\n";
    let long = format!("{}\nsay 4;\n", "# filler comment line\n".repeat(200));

    let expected_short = mutsu::dump_ast(short).expect("short parses");
    for _ in 0..25 {
        mutsu::dump_ast(&long).expect("long parses");
        mutsu::dump_ast(with_nested).expect("nested parses");
        let again = mutsu::dump_ast(short).expect("short parses");
        assert_eq!(
            expected_short, again,
            "line-number state leaked from a previous parse into a later one"
        );
    }

    // `SetLine` markers are the only positions mutsu has (ADR-0065 D6), so pin
    // that they are actually present and correct rather than collapsed to 1.
    let setline_markers = expected_short.matches("SetLine(").count();
    assert_eq!(
        setline_markers, 2,
        "expected one SetLine marker per statement, got {setline_markers}:\n{expected_short}"
    );
}

/// The same gate, on the entry point a language server actually calls.
/// `mutsu::analysis::check` parses *and* runs the CHECK-time undeclared-routine
/// analysis against a freshly constructed `Interpreter` — more per-call work
/// than `dump_ast`, and therefore the one that has to hold up under repetition.
#[test]
fn repeated_analysis_of_an_unchanged_document_is_stable() {
    let _guard = exclusive();
    let n = iterations();

    let baseline = mutsu::analysis::check(DOCUMENT);
    for _ in 0..2 {
        mutsu::analysis::check(DOCUMENT);
    }

    let symbols_before = mutsu::symbol::interned_count();
    let rss_before = rss_kib();
    let started = Instant::now();
    for _ in 0..n {
        let got = mutsu::analysis::check(DOCUMENT);
        assert_eq!(
            baseline, got,
            "re-analysing an unchanged document produced a different report"
        );
    }
    let elapsed = started.elapsed();
    let symbol_growth = mutsu::symbol::interned_count() - symbols_before;
    let rss_after = rss_kib();

    println!("--- ADR-0065 S0 probe: {n} analysis::check calls ---");
    println!(
        "  wall clock      : {elapsed:?} total, {:?} per check",
        elapsed / n as u32
    );
    println!(
        "  interned symbols: +{symbol_growth} ({:.2}/check)",
        symbol_growth as f64 / n as f64
    );
    if let (Some(before), Some(after)) = (rss_before, rss_after) {
        println!(
            "  resident memory : {:+} KiB ({:.3} KiB/check)",
            after as isize - before as isize,
            (after as f64 - before as f64) / n as f64,
        );
    }

    // ZERO, not "one per anonymous declaration per pass". An analysis-only
    // parse mints its anonymous registry names from UNIT-LOCAL counters
    // (`mutsu::anon_names`), so re-analysing the same document re-uses the same
    // names instead of leaking a fresh interned one every time -- which was the
    // only unbounded component S0 originally found. The plain-parse probe above
    // keeps the process-global counters and still grows by 1.00/parse, which is
    // correct: names it mints CAN reach the registry.
    assert_eq!(
        symbol_growth,
        0,
        "analysing an unchanged document interned {symbol_growth} new names over {n} checks \
         ({:.2}/check); an analysis parse must mint no process-global names at all",
        symbol_growth as f64 / n as f64,
    );
    if let (Some(before), Some(after)) = (rss_before, rss_after) {
        let growth = after.saturating_sub(before);
        assert!(
            growth < 32 * 1024,
            "resident memory grew {growth} KiB over {n} analyses of an unchanged document"
        );
    }
}

/// The other analysis entry point. `symbols` runs its own recovering parse, and
/// a server calls it on every keystroke, so its leak would have been the larger
/// of the two. Same rule, same zero.
#[test]
fn repeated_symbol_outlines_of_an_unchanged_document_intern_nothing() {
    let _guard = exclusive();
    let n = iterations();

    let baseline = mutsu::analysis::symbols(DOCUMENT);
    for _ in 0..2 {
        mutsu::analysis::symbols(DOCUMENT);
    }

    let symbols_before = mutsu::symbol::interned_count();
    for _ in 0..n {
        let got = mutsu::analysis::symbols(DOCUMENT);
        assert_eq!(
            baseline.len(),
            got.len(),
            "re-outlining an unchanged document produced a different symbol count"
        );
    }
    let symbol_growth = mutsu::symbol::interned_count() - symbols_before;

    println!("--- ADR-0065 S0 probe: {n} analysis::symbols calls ---");
    println!(
        "  interned symbols: +{symbol_growth} ({:.2}/call)",
        symbol_growth as f64 / n as f64
    );
    assert_eq!(
        symbol_growth, 0,
        "outlining an unchanged document interned {symbol_growth} new names over {n} calls"
    );
}

/// The other half of the rule: the unit-local mode is OFF for every existing
/// caller. A plain parse's anonymous names can reach the registry, so they must
/// stay process-unique — this pins that the mode did not leak out of the
/// analysis entry points and start handing two compilation units the same
/// `__ANON_CLASS_0__`.
#[test]
fn a_plain_parse_still_mints_process_global_anonymous_names() {
    let _guard = exclusive();

    let before = mutsu::symbol::interned_count();
    for _ in 0..4 {
        mutsu::dump_ast(DOCUMENT).expect("parses");
    }
    let growth = mutsu::symbol::interned_count() - before;
    assert!(
        growth >= 4,
        "a plain parse minted only {growth} names over 4 parses of a document with an \
         anonymous class; the process-global counters must still advance once per parse"
    );
}

/// D8's open question: whether documents can be analysed on more than one
/// thread. The parser's working state (`SCOPES`, the memo tables,
/// `ORIGINAL_SOURCE`) is thread-local and the symbol table is behind an
/// `RwLock`, so concurrent parsing of *different* documents should hold. This
/// pins that, so a future change that moves parser state to a process-global
/// fails here rather than in a server under load.
#[test]
fn documents_parse_independently_on_separate_threads() {
    let _guard = exclusive();
    let sources: Vec<String> = (0..4).map(|i| format!("{DOCUMENT}\nsay {i};\n")).collect();
    let expected: Vec<String> = sources
        .iter()
        .map(|s| normalize_generated_ids(&mutsu::dump_ast(s).expect("parses")))
        .collect();

    for _round in 0..5 {
        let handles: Vec<_> = sources
            .iter()
            .cloned()
            .map(|src| {
                std::thread::spawn(move || {
                    normalize_generated_ids(&mutsu::dump_ast(&src).expect("parses"))
                })
            })
            .collect();
        for (i, handle) in handles.into_iter().enumerate() {
            let got = handle.join().expect("parser thread must not panic");
            assert_eq!(
                expected[i], got,
                "document {i} parsed differently on a worker thread than on the main thread"
            );
        }
    }
}
