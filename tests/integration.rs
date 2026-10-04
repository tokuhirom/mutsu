//! The single integration-test binary for every `tests/*.rs` file that does
//! not install a `#[global_allocator]` of its own.
//!
//! Each file used to be its own test target, and every target links the whole
//! `mutsu` library: forty-odd links of a very large crate per `cargo test`,
//! which dominated the debug build on a small box. The files stay where they
//! were (a `mod` in a crate root at `tests/` resolves to `tests/<name>.rs`),
//! so their paths, quoted throughout `src/` and `docs/`, remain valid; only
//! the target they compile into changed. The allocation-budget tests, which
//! each need a counting global allocator, share `tests/alloc_budget.rs`.
//!
//! `Cargo.toml` sets `autotests = false` so these files are not also built
//! as targets of their own; `make check-integration-tests` fails when a
//! `tests/*.rs` file is declared by neither root, which would otherwise make
//! a new test silently never run.

mod profile_doc;

mod adr0044_listop_fast_path;
mod carrier_compile_cache_keyed_by_parse_site;
mod carrier_compile_cache_serves_whenever_callbacks;
mod closure_call_intern_budget;
mod crash_report;
mod dispatch_poll_placement;
mod dynamic_method_intern_budget;
mod flaky_retry;
mod gc_parallel_interpreters;
mod gc_stress;
mod issue_7228;
mod jit_diff;
mod lazy_match_no_eager_materialization;
mod lazy_match_truthiness_no_materialization;
mod long_lived_parse;
mod multi_call_resolves_once;
mod multi_candidate_match_does_not_copy_the_frame_env;
mod mzef_shim;
mod named_call_intern_budget;
mod nested_sub_in_block_no_otf_recompile;
mod param_default_literal_binds_directly;
mod profile_allocations;
mod profile_counts;
mod profile_regions;
mod profile_samples;
mod program_tables_shared_across_thread_clones;
mod proto_method_body_compiled_once;
mod regex_embedded_code_compiled_once;
mod regex_engine_corpus;
mod regex_match_intern_budget;
mod regex_parse_cache;
mod regex_prefilter_differential;
mod regex_prefilter_engagement;
mod regex_proto_candidate_raw_cache;
mod regex_subject_materialized_once;
mod registry_cow_not_paid_per_supply_registration;
mod repl_routine;
mod routine_package_switch_budget;
mod stash_bind_key;
mod statement_call_resolves_once;
mod static_operator_intern_budget;
mod unrelated_bind_keeps_local_read_fast_path;
