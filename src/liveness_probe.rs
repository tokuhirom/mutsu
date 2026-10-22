//! TEMPORARY measurement aid (never merged): which test files reach a block,
//! and a switch that skips one block, for ADR-11276 slice 5's liveness probe.

use std::collections::HashSet;
use std::sync::{Mutex, OnceLock};

fn skip_id() -> &'static Option<String> {
    static S: OnceLock<Option<String>> = OnceLock::new();
    S.get_or_init(|| std::env::var("MUTSU_PROBE_SKIP").ok())
}

pub(crate) fn skip(id: &str) -> bool {
    skip_id()
        .as_deref()
        .is_some_and(|list| list.split(',').any(|s| s == id))
}

pub(crate) fn hit(id: &'static str) {
    static DIR: OnceLock<Option<String>> = OnceLock::new();
    static SEEN: OnceLock<Mutex<HashSet<&'static str>>> = OnceLock::new();
    let Some(dir) = DIR.get_or_init(|| std::env::var("MUTSU_PROBE_DIR").ok()) else {
        return;
    };
    let seen = SEEN.get_or_init(|| Mutex::new(HashSet::new()));
    if !seen.lock().unwrap().insert(id) {
        return;
    }
    use std::io::Write;
    let args: Vec<String> = std::env::args().skip(1).collect();
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(format!("{dir}/{id}.hits"))
    {
        let _ = writeln!(f, "{}", args.join(" "));
    }
}
