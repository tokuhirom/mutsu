//! Frame-lexical routines (ADR-0113).
//!
//! A `my sub` declared directly in a routine body used to be installed into
//! the program-global routine registry on every call of that routine and
//! taken out again on return (the registry snapshot/restore that makes it
//! lexical). That cost the enclosing routine every light call path
//! (`has_inner_subs`), moved `fn_resolve_gen` twice per call, and resolved
//! every call to the inner sub by name through the registry.
//!
//! When the only things the enclosing body ever does with such a sub are to
//! call it by its bare name and read it as a bare `&name`, none of that is
//! observable: nothing can see the registry entry, and a `&name` read is
//! served by the chunk's lexical table (the code object is built at the point
//! of use). This pass proves that from the body's AST and bytecode, then
//!
//! - marks the declaration plans `frame_lexical`, so executing them derives
//!   the routine's definition once per interpreter and registers nothing;
//! - lists the routine in [`CompiledCode::lexical_routines`] of every chunk
//!   that calls it or reads `&name`, so those call sites dispatch straight to
//!   the routine's compiled body without any name resolution, and `&name`
//!   builds the code object from the routine's derived definition.
//!
//! Anything the proof cannot cover leaves the declaration exactly as before.

use super::frame_lexical_ast_scan::AstScan;
use super::*;
use crate::opcode::{CompiledCode, FrameLexicalRef};
use crate::value::ValueView;
use std::collections::{HashMap, HashSet};

/// Parameter traits the plain call paths bind themselves.
const PLAIN_PARAM_TRAITS: &[&str] = &["copy", "rw", "raw", "readonly"];

/// Every chunk reachable from `code` through nested closures, depth first.
pub(super) fn visit_code(code: &CompiledCode, f: &mut impl FnMut(&CompiledCode)) {
    f(code);
    for nested in &code.closure_compiled_codes {
        visit_code(nested, f);
    }
}

/// [`visit_code`] with mutable access. A nested closure chunk is copied out
/// of its `Arc` only when `wants` says `f` has something to change in it.
pub(super) fn visit_code_mut(
    code: &mut CompiledCode,
    wants: &impl Fn(&CompiledCode) -> bool,
    f: &mut impl FnMut(&mut CompiledCode),
) {
    if wants(code) {
        f(code);
    }
    for nested in &mut code.closure_compiled_codes {
        let mut any = false;
        visit_code(nested, &mut |c| any |= wants(c));
        if any {
            visit_code_mut(std::sync::Arc::make_mut(nested), wants, f);
        }
    }
}

/// Names of candidate routines this chunk calls by bare name or reads as a
/// bare `&name` code object (both served by [`CompiledCode::lexical_routines`]),
/// and the ones it references in a way the lexical table cannot serve.
pub(super) fn scan_chunk(
    code: &CompiledCode,
    names: &HashSet<Symbol>,
) -> (Vec<Symbol>, Vec<Symbol>) {
    let mut called = Vec::new();
    let mut bad = Vec::new();
    let const_sym = |idx: u32| -> Option<Symbol> {
        match code.constants.get(idx as usize).map(Value::view) {
            Some(ValueView::Str(s)) => {
                let s = s.as_str();
                let bare = s.strip_prefix('&').unwrap_or(s);
                let bare = bare.rsplit("::").next().unwrap_or(bare);
                Symbol::lookup(bare).filter(|sym| names.contains(sym))
            }
            _ => None,
        }
    };
    for op in &code.ops {
        // A statically linked call carries its callee in its site, not in a
        // name constant. Its cold fallback reaches a frame lexical through
        // this chunk's lexical table, so it counts as a bare call.
        if let OpCode::CallTrir { site, .. } = op {
            if let Some(s) = code.trir_call_sites.get(*site as usize)
                && names.contains(&s.name)
            {
                called.push(s.name);
            }
            continue;
        }
        let callee = match op {
            OpCode::ExecCallPairs { name_idx, .. } => Some(*name_idx),
            _ => CompiledCode::op_callee_name_const_idx(op),
        };
        if let Some(idx) = callee {
            if let Some(sym) = const_sym(idx) {
                let exact = matches!(
                    code.constants.get(idx as usize).map(Value::view),
                    Some(ValueView::Str(s)) if s.as_str() == sym.as_str()
                );
                if exact {
                    called.push(sym)
                } else {
                    bad.push(sym)
                }
            }
            continue;
        }
        // `&name` as a value: the lexical table builds the code object.
        if let OpCode::GetCodeVar(idx) = op
            && let Some(sym) = const_sym(*idx)
            && matches!(
                code.constants.get(*idx as usize).map(Value::view),
                Some(ValueView::Str(s)) if s.as_str() == sym.as_str()
            )
        {
            called.push(sym);
            continue;
        }
        // A variable op naming the bare `name` reads or writes `$name`, a
        // different symbol than the routine `&name`.
        if let Some(idx) = CompiledCode::op_name_const_idx(op)
            && matches!(
                code.constants.get(idx as usize).map(Value::view),
                Some(ValueView::Str(s)) if Symbol::lookup(s.as_str()).is_some_and(|sym| names.contains(&sym))
            )
        {
            continue;
        }
        let other = CompiledCode::op_code_var_read_const_idx(op)
            .or_else(|| CompiledCode::op_name_const_idx(op))
            .or(match op {
                OpCode::LoadConst(idx) => Some(*idx),
                _ => None,
            });
        if let Some(sym) = other.and_then(const_sym) {
            bad.push(sym);
        }
    }
    // A body stashed as AST and compiled at run time (a `gather` or
    // `whenever` body) would compile its calls without this chunk's table.
    // A closure literal's pool slot is exempt when every op that creates it
    // carries its compiled chunk: that chunk is what runs, and it is visited
    // (and equipped) as one of `closure_compiled_codes`.
    if !code.stmt_pool.is_empty() {
        let mut compiled_slots: HashSet<u32> = HashSet::new();
        let mut ast_slots: HashSet<u32> = HashSet::new();
        for op in &code.ops {
            if let OpCode::MakeAnonSub(idx, cc, _)
            | OpCode::MakeAnonSubParams(idx, cc, _)
            | OpCode::MakeLambda(idx, cc, _)
            | OpCode::MakeBlockClosure(idx, cc) = op
            {
                if cc.is_some() {
                    compiled_slots.insert(*idx);
                } else {
                    ast_slots.insert(*idx);
                }
            }
        }
        for (idx, stmt) in code.stmt_pool.iter().enumerate() {
            let idx = idx as u32;
            if compiled_slots.contains(&idx) && !ast_slots.contains(&idx) {
                continue;
            }
            let Ok(json) = serde_json::to_string(stmt) else {
                bad.extend(names.iter().copied());
                continue;
            };
            for sym in names {
                if json.contains(&format!("\"{}\"", sym.as_str())) {
                    bad.push(*sym);
                }
            }
        }
    }
    (called, bad)
}

impl Compiler {
    /// The `my sub` declarations directly in `body` that are shaped for the
    /// frame-lexical treatment on their own (no traits, no multi, a plain
    /// name that no builtin answers to, not the body's value).
    fn frame_lexical_candidates(&self, body: &[Stmt]) -> Vec<Symbol> {
        let last_decl = match body.iter().rev().find(|s| !matches!(s, Stmt::SetLine(..))) {
            Some(Stmt::SubDecl { name, .. }) => Some(*name),
            _ => None,
        };
        let mut out = Vec::new();
        for stmt in body {
            let Stmt::SubDecl {
                name,
                name_expr: None,
                param_defs,
                associativity: None,
                precedence_trait: None,
                signature_alternates,
                multi: false,
                is_rw: false,
                is_raw: false,
                is_export: false,
                export_tags,
                is_test_assertion: false,
                supersede: false,
                custom_traits,
                ..
            } = stmt
            else {
                continue;
            };
            let s = name.as_str();
            let plain = s.starts_with(|c: char| c.is_alphabetic() || c == '_')
                && s.chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''));
            if !plain
                || !signature_alternates.is_empty()
                || !export_tags.is_empty()
                || !custom_traits.is_empty()
                || Some(*name) == last_decl
                || self.lexical_dup_routines.contains(s)
                || crate::runtime::Interpreter::is_builtin_function(s)
                || param_defs.iter().any(|pd| {
                    pd.traits
                        .iter()
                        .any(|t| !PLAIN_PARAM_TRAITS.contains(&t.as_str()))
                })
            {
                continue;
            }
            out.push(*name);
        }
        out
    }

    /// The pass itself; see the module doc comment. Runs on a routine body's
    /// compiler once the body is compiled, before its chunk is finalized.
    pub(super) fn resolve_frame_lexical_routines(&mut self, body: &[Stmt]) {
        let candidates = self.frame_lexical_candidates(body);
        if candidates.is_empty() {
            return;
        }
        let Ok(json) = serde_json::to_value(body) else {
            return;
        };
        let mut scan = AstScan {
            names: candidates.iter().map(|n| n.as_str().to_string()).collect(),
            ..AstScan::default()
        };
        scan.walk(&json, None, None);
        if scan.reject_all {
            return;
        }
        let mut names: HashSet<Symbol> = candidates
            .into_iter()
            .filter(|n| {
                let s = n.as_str();
                !scan.rejected.contains(s) && scan.decls.get(s) == Some(&1)
            })
            .collect();
        if names.is_empty() {
            return;
        }

        // Each name's declaration plans (the hoisted and the in-sequence
        // registration) must agree on one compiled body.
        let mut refs: HashMap<Symbol, (FrameLexicalRef, Symbol)> = HashMap::new();
        for name in names.clone() {
            let keys: Vec<&Vec<Symbol>> = self
                .code
                .sub_decl_plans
                .iter()
                .filter(|p| p.name == name)
                .map(|p| &p.compiled_routine_keys)
                .collect();
            let key = match keys.first() {
                Some(k) if k.len() == 1 && keys.iter().all(|other| *other == *k) => k[0],
                _ => {
                    names.remove(&name);
                    continue;
                }
            };
            let Some(cf) = self.compiled_functions.get(&key) else {
                names.remove(&name);
                continue;
            };
            // `state`/`once` storage is keyed by the routine's registration
            // clone, which a frame-lexical routine never gets.
            let mut stateful = cf.is_cached;
            visit_code(&cf.code, &mut |c| {
                stateful |= !c.state_locals.is_empty() || c.has_once;
            });
            if stateful {
                names.remove(&name);
                continue;
            }
            let r = FrameLexicalRef {
                name,
                package: Symbol::intern(&cf.package),
                fingerprint: cf.fingerprint,
            };
            refs.insert(name, (r, key));
        }
        if names.is_empty() {
            return;
        }

        // Bodies this pass can equip with a lexical call table: the routine
        // body itself (and its closures) and every routine declared in it.
        // Anything else that mentions a candidate disqualifies it.
        let owned: HashSet<Symbol> = self
            .code
            .sub_decl_plans
            .iter()
            .flat_map(|p| p.compiled_routine_keys.iter().copied())
            .collect();
        let mut bad: HashSet<Symbol> = HashSet::new();
        visit_code(&self.code, &mut |c| bad.extend(scan_chunk(c, &names).1));
        for (key, cf) in self.compiled_functions.iter() {
            visit_code(&cf.code, &mut |c| {
                let (called, wrong) = scan_chunk(c, &names);
                bad.extend(wrong);
                if !owned.contains(key) {
                    bad.extend(called);
                }
            });
        }
        names.retain(|n| !bad.contains(n));
        if names.is_empty() {
            return;
        }
        let final_refs: Vec<FrameLexicalRef> = names.iter().map(|n| refs[n].0).collect();

        for plan in &mut self.code.sub_decl_plans {
            if names.contains(&plan.name) {
                plan.frame_lexical = Some(refs[&plan.name].0);
                plan.frame_lexical_value = scan.values.contains(plan.name.as_str());
            }
        }
        let wants = |c: &CompiledCode| !scan_chunk(c, &names).0.is_empty();
        let mut equip = |c: &mut CompiledCode| {
            for sym in scan_chunk(c, &names).0 {
                if let Some(r) = final_refs.iter().find(|r| r.name == sym)
                    && !c.lexical_routines.contains(r)
                {
                    c.lexical_routines.push(*r);
                }
            }
        };
        visit_code_mut(&mut self.code, &wants, &mut equip);
        super::frame_lexical_inherit::mark_lexical_subtree(&mut self.code);
        let owned_callers: Vec<Symbol> = owned
            .iter()
            .copied()
            .filter(|key| {
                self.compiled_functions.get(key).is_some_and(|cf| {
                    let mut any = false;
                    visit_code(&cf.code, &mut |c| any |= wants(c));
                    any
                })
            })
            .collect();
        for key in owned_callers {
            if let Some(cf) = self.compiled_functions.make_mut(&key) {
                visit_code_mut(&mut cf.code, &wants, &mut equip);
                super::frame_lexical_inherit::mark_lexical_subtree(&mut cf.code);
            }
        }
    }
}
