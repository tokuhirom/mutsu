//! The differential oracle of the cut-over (ADR-15334 slice 9): for every file of
//! a list, the lowered tree of the default round trip (leaves) must equal the
//! lowered tree of the convert level (every node through `convert` and `lower`).
//! A family may be carried as a leaf only while the two agree.
//!
//! `ORACLE_FILES=list.txt cargo test --lib oracle_corpus -- --ignored --nocapture`

use crate::ast::Stmt;
use crate::rakuast::frontend::{CONVERT_OVERRIDE, round_trip};

fn shape(stmts: &[Stmt]) -> Vec<String> {
    stmts
        .iter()
        .filter(|s| !matches!(s, Stmt::SetLine(..)))
        .map(|s| format!("{s:?}"))
        .collect()
}

fn lowered(stmts: &[Stmt], convert: bool) -> Result<Vec<String>, String> {
    CONVERT_OVERRIDE.with(|c| c.set(Some(convert)));
    let out = round_trip(stmts);
    CONVERT_OVERRIDE.with(|c| c.set(None));
    out.map(|s| shape(&s)).map_err(|e| e.message)
}

#[test]
#[ignore]
fn oracle_corpus() {
    let Ok(list) = std::env::var("ORACLE_FILES") else {
        return;
    };
    let mut diverged = 0;
    for path in std::fs::read_to_string(list).unwrap().lines() {
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        let Ok((spelled, _)) = crate::parse_dispatch::parse_source_spelled(&source) else {
            continue;
        };
        let leaf = lowered(&spelled, false);
        let full = lowered(&spelled, true);
        match (leaf, full) {
            (Ok(a), Ok(b)) if a == b => {}
            (Ok(a), Ok(b)) => {
                diverged += 1;
                let at = a.iter().zip(&b).position(|(x, y)| x != y).unwrap_or(0);
                let cut = |s: Option<&String>| s.map(|s| s.chars().take(300).collect::<String>());
                println!(
                    "DIVERGE {path} at {at}\n  leaf:    {:?}\n  convert: {:?}",
                    cut(a.get(at)),
                    cut(b.get(at))
                );
            }
            (a, b) => println!("ERR {path}: leaf={:?} convert={:?}", a.err(), b.err()),
        }
    }
    println!("diverged: {diverged}");
}
