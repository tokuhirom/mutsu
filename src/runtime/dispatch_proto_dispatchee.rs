//! `Routine::Dispatcher.add_dispatchee`: adding candidates to a `proto` after
//! its declaration (ecosystem `JSON::Fast::Hyper`, `CLI::Version`,
//! `shorten-sub-commands`).
use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `&proto.add_dispatchee(&code)`: make `other` a candidate of the proto
    /// `package::name`.
    ///
    /// A code VALUE (`ValueView::Sub` — an anonymous `sub (...) { }`, a
    /// closure, or a named routine's `&name`) is held by the proto as that
    /// value (#10929): its registry row carries the value in
    /// [`FunctionDef::dispatchee`], selection ranks the row by its signature
    /// like any declared candidate, and running it calls the value, so the
    /// candidate keeps the lexicals it closed over. A materialized multi
    /// dispatcher contributes each of its captured candidates. A bare
    /// `Routine` handle (no code value behind it) still copies the routine's
    /// registry rows.
    // Cost: O(r + c * a), r = registry size (scan for a `Routine` handle's rows), c = candidates
    // of `other`, a = parameters per candidate.
    pub(super) fn add_routine_dispatchee(
        &mut self,
        package: &str,
        name: &str,
        other: &Value,
    ) -> Result<(), RuntimeError> {
        let proto_key = crate::qualified::qualified(Symbol::intern(package), Symbol::intern(name));
        let proto_key = proto_key.as_str();
        let other = match other.view() {
            ValueView::Mixin(inner, _) if matches!(inner.view(), ValueView::Sub(_)) => {
                inner.as_ref()
            }
            _ => other,
        };
        match other.view() {
            ValueView::Sub(data) => {
                let candidates = data
                    .env
                    .get_sym(crate::symbol::well_known::multi_dispatch_candidates())
                    .cloned()
                    .and_then(Value::into_array);
                match candidates {
                    Some((candidates, _)) => {
                        for candidate in candidates.iter() {
                            if matches!(candidate.view(), ValueView::Sub(_)) {
                                self.add_value_dispatchee(proto_key, name, candidate);
                            }
                        }
                    }
                    None => self.add_value_dispatchee(proto_key, name, other),
                }
                Ok(())
            }
            ValueView::Routine {
                name: oname,
                package: opkg,
                ..
            } => self.add_registered_dispatchee(proto_key, opkg, oname),
            _ => Err(RuntimeError::new(
                "add_dispatchee requires a Routine argument",
            )),
        }
    }

    /// Register the code value `code` as a candidate row of `proto_key`.
    // Cost: O(a + b), a = parameters of `code`, b = its body (one fingerprint hash).
    fn add_value_dispatchee(&mut self, proto_key: &str, proto_name: &str, code: &Value) {
        let ValueView::Sub(data) = code.view() else {
            return;
        };
        // Two closures cloned from one literal share params and body; the
        // value's identity keeps them distinct candidates, as in Rakudo, where
        // the multi-registration dedup would otherwise fold them into one.
        let fingerprint = {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            crate::ast::function_body_fingerprint(&data.params, &data.param_defs, &data.body)
                .hash(&mut hasher);
            data.id.hash(&mut hasher);
            hasher.finish()
        };
        let body_fp_cache = std::sync::OnceLock::new();
        let _ = body_fp_cache.set(fingerprint);
        let def = FunctionDef {
            package: data.package,
            name: Symbol::intern(proto_name),
            params: (*data.params).clone(),
            param_defs: (*data.param_defs).clone(),
            // Never run: the row runs `dispatchee`. Selection reads only the
            // signature, and the fingerprint is seeded above.
            body: Vec::new(),
            is_test_assertion: false,
            is_implementation_detail: false,
            is_cached: false,
            is_rw: data.is_rw,
            is_raw: data.is_raw,
            declarator: crate::ast::RoutineDeclarator::Sub,
            empty_sig: false,
            is_stub: false,
            return_type: None,
            is_default: false,
            deprecated_message: None,
            source_file: None,
            source_line: None,
            decl_order: crate::runtime::resolution::next_decl_order(),
            compiled: None,
            dispatchee: Some(code.clone()),
            body_fp_cache,
            captured_readonly: None,
            body_facts_cache: std::sync::OnceLock::new(),
        };
        let key = Self::dispatchee_row_key(proto_key, &def);
        self.insert_multi_overload(&key, def);
    }

    /// Copy every registry row of the routine `opkg::oname` into `proto_key`.
    // Cost: O(r + c * a), r = registry size, c = rows of the routine, a = parameters per row.
    fn add_registered_dispatchee(
        &mut self,
        proto_key: &str,
        opkg: Symbol,
        oname: Symbol,
    ) -> Result<(), RuntimeError> {
        let single = crate::qualified::qualified(opkg, oname);
        let single = single.as_str();
        let multi_prefix = format!("{single}/");
        let defs: Vec<_> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, _)| {
                let k = k.resolve();
                k == single || k.starts_with(&multi_prefix)
            })
            .map(|(_, d)| d.clone())
            .collect();
        if defs.is_empty() {
            return Err(RuntimeError::new(format!(
                "Cannot add dispatchee: no routine named '{oname}' found"
            )));
        }
        for def in defs {
            let key = Self::dispatchee_row_key(proto_key, &def);
            self.insert_multi_overload(&key, (*def).clone());
        }
        Ok(())
    }

    /// The multi-candidate registry key `def` is filed under in `proto_key`:
    /// `proto/arity` or, with any typed positional, `proto/arity:T1,T2`.
    // Cost: O(a), a = parameters of `def`.
    fn dispatchee_row_key(proto_key: &str, def: &FunctionDef) -> String {
        let positional: Vec<_> = def
            .param_defs
            .iter()
            .filter(|p| {
                !p.named && (!p.slurpy || p.name == "_capture") && !p.is_capture_subsignature()
            })
            .collect();
        let types: Vec<&str> = positional
            .iter()
            .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
            .collect();
        if types.iter().any(|t| *t != "Any") {
            format!("{proto_key}/{}:{}", positional.len(), types.join(","))
        } else {
            format!("{proto_key}/{}", positional.len())
        }
    }
}
