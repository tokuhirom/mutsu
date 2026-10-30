//! Survey of what the executing round trip still converts (ADR-15334 slice 9):
//! counts, per statement variant, the statements of a file list that
//! the purity test refuses, so the cut-over starts from the largest family.
//!
//! `ORACLE_FILES=list.txt cargo test --lib convert_survey -- --ignored --nocapture`

use std::collections::BTreeMap;

use crate::ast::Stmt;
use crate::ast_visit::{Visit, walk_stmt};

#[derive(Default)]
struct Survey(BTreeMap<String, (usize, usize)>);

impl<'ast> Visit<'ast> for Survey {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        let name = format!("{stmt:?}");
        let name = name
            .split(|c: char| !c.is_alphanumeric())
            .next()
            .unwrap_or("")
            .to_string();
        let entry = self.0.entry(name).or_default();
        if super::pure::analyse_stmt(stmt).is_some() {
            entry.0 += 1;
        } else {
            entry.1 += 1;
        }
        walk_stmt(self, stmt);
    }
}

#[test]
#[ignore]
fn convert_survey() {
    let Ok(list) = std::env::var("ORACLE_FILES") else {
        return;
    };
    let run = move || {
        let mut survey = Survey::default();
        for path in std::fs::read_to_string(list).unwrap().lines() {
            let Ok(source) = std::fs::read_to_string(path) else {
                continue;
            };
            let Ok((spelled, _)) = crate::parse_dispatch::parse_source_spelled(&source) else {
                continue;
            };
            for stmt in &spelled {
                survey.visit_stmt(stmt);
            }
        }
        let mut rows: Vec<_> = survey.0.into_iter().filter(|(_, (_, n))| *n > 0).collect();
        rows.sort_by_key(|(_, (_, n))| std::cmp::Reverse(*n));
        for (name, (carried, converted)) in rows {
            println!("{name}\tcarried={carried}\tconverted={converted}");
        }
    };
    std::thread::Builder::new()
        .stack_size(512 << 20)
        .spawn(run)
        .unwrap()
        .join()
        .unwrap();
}
