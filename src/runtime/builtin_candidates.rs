//! `.candidates` / `.cando` of a built-in routine (#12472).
//!
//! A built-in has no registered `FunctionDef`, so `routine_candidate_subs`
//! finds nothing for it. This builds one `Sub` code object per candidate Rakudo
//! declares (`builtins::rakudo_candidates`), carrying that candidate's
//! signature, `multi` flag and `SETTING::` declaration site, so a caller can
//! `cando`-filter the candidates and ask each for `.signature`, `.file` and
//! `.line`.

use super::*;
use crate::builtins::rakudo_candidates::{CoreCandidate, core_method_candidates, core_sub_candidates};

impl Interpreter {
    /// The candidate code objects of the built-in routine `package`/`name`, or
    /// empty when it is not a core routine or the program declares its own
    /// routine of that name (which then owns the name).
    // Cost: O(c * s), c = candidates, s = signature length (each is parsed).
    pub(super) fn builtin_routine_candidates(&self, package: &str, name: &str) -> Vec<Value> {
        if !self.routine_candidate_defs(package, name).is_empty() {
            return Vec::new();
        }
        let is_global = package.is_empty() || crate::qualified::is_global_name(package);
        let rows = if is_global {
            core_sub_candidates(name)
        } else {
            core_method_candidates(package, name)
        };
        rows.iter()
            .filter_map(|row| self.builtin_candidate_code(package, name, row))
            .collect()
    }

    fn builtin_candidate_code(
        &self,
        package: &str,
        name: &str,
        row: &CoreCandidate,
    ) -> Option<Value> {
        let sig = normalize_core_signature(row.signature);
        let sig = sig.strip_prefix(':').unwrap_or(&sig);
        let die = format!(
            "die 'Built-in candidate of {} is not directly invocable'",
            name.replace('\'', "")
        );
        let is_method = !(package.is_empty() || crate::qualified::is_global_name(package));
        // The `:` invocant marker is only legal in a method's signature.
        let src = if is_method {
            format!("class __BuiltinCandidate {{ method __builtin_candidate {sig} {{ {die} }} }}")
        } else {
            format!("sub __builtin_candidate {sig} {{ {die} }}")
        };
        let (stmts, _) = crate::parser::parse_program(&src).ok()?;
        let (params, param_defs, body) = find_routine_decl(stmts)?;
        let mut env = self.env.clone();
        if row.multi {
            env.insert("__mutsu_is_multi_candidate".to_string(), Value::TRUE);
        }
        let code = Value::make_sub_for_routine(
            Symbol::intern(package),
            Symbol::intern(name),
            params,
            param_defs,
            body,
            false,
            env,
            None,
        );
        if let ValueView::Sub(data) = code.view() {
            let mut data = (**data).clone();
            data.source_line = Some(row.line);
            data.source_file = Some(row.file.to_string());
            return Some(Value::sub_value(crate::gc::Gc::new(data)));
        }
        Some(code)
    }
}

/// Rakudo prints a method's invocant as `Str:D $:: ...` (the `$:` marker
/// followed by the `:` that ends it) and an `is rw` invocant as
/// `$x: is rw:`; mutsu's signature parser takes the plain `Str:D $: ...`
/// form, and has no `is item` parameter trait.
// Cost: O(n), n = signature length.
fn normalize_core_signature(sig: &str) -> String {
    sig.replace(":: ", ": ")
        .replace("::)", ":)")
        .replace(": is rw: ", ": ")
        .replace(": is rw:)", ":)")
        .replace(" is item", "")
}

/// The parameters and body of the first `sub`/`method` declaration in `stmts`
/// or in the body of a class declared there.
// Cost: O(n), n = statements scanned.
fn find_routine_decl(stmts: Vec<Stmt>) -> Option<(Vec<String>, Vec<ParamDef>, Vec<Stmt>)> {
    let decl = |stmt: Stmt| match stmt {
        Stmt::SubDecl {
            params,
            param_defs,
            body,
            ..
        }
        | Stmt::MethodDecl {
            params,
            param_defs,
            body,
            ..
        } => Some((params, param_defs, body)),
        _ => None,
    };
    let (classes, others): (Vec<Stmt>, Vec<Stmt>) = stmts
        .into_iter()
        .partition(|stmt| matches!(stmt, Stmt::ClassDecl { .. }));
    others.into_iter().find_map(decl).or_else(|| {
        classes.into_iter().find_map(|class| match class {
            Stmt::ClassDecl { body, .. } => body.into_iter().find_map(decl),
            _ => None,
        })
    })
}
