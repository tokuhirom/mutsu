//! `Routine`'s `is_dispatcher`, `dispatcher`, `multi` and `package` (ADR-11276
//! §9.121), for a `Sub` and for a `&name` handle on a routine the registry
//! holds (`ValueView::Routine`). The rows are in
//! `builtins/method_table/instances/routine.rs`; each calls one method here.

use super::*;

impl Interpreter {
    /// Whether the routine `name` of `package` is a multi: it has a proto or
    /// multi candidates.
    // Cost: O(c), c = the multi candidates it looks up.
    fn routine_name_is_multi(&mut self, package: &str, name: &str) -> bool {
        let qualified = crate::qualified::qualified_text(package, name)
            .as_str()
            .to_string();
        self.resolve_proto_function(&qualified).is_some()
            || self.resolve_proto_function_with_alias(name).is_some()
            || self.has_multi_candidates(name)
    }

    /// `Routine.is_dispatcher`. A `Routine` handle names a whole multi, so it
    /// *is* the dispatcher. A `multi sub`'s dispatcher is the `Sub` carrying
    /// its captured candidates; each `.candidates` entry carries none, and an
    /// ordinary routine answers `False`.
    // Cost: O(c), c = multi candidates, for a handle; O(1) for a `Sub`.
    pub(crate) fn routine_is_dispatcher(&mut self, target: &Value) -> Option<Value> {
        match target.view() {
            ValueView::Routine { name, package, .. } => Some(Value::truth(
                self.routine_name_is_multi(&package.resolve(), &name.resolve()),
            )),
            ValueView::Sub(data) => Some(Value::truth(
                Self::sub_multi_method_dispatcher_name(&data).is_some()
                    || data.env.contains_key("__mutsu_multi_dispatch_candidates"),
            )),
            _ => None,
        }
    }

    /// `Routine.onlystar`: whether the routine is a `proto` whose body is the
    /// onlystar `{*}`. A `&name` handle names a user proto when the registry
    /// holds one (its body decides) and a built-in routine otherwise, which
    /// Rakudo declares as a proto of its candidates (True). A `Sub` is one only
    /// when it is the dispatcher of a multi and its body is `{*}`.
    // Cost: O(c), c = multi candidates (the proto lookup).
    pub(crate) fn routine_onlystar(&mut self, target: &Value) -> Option<Value> {
        match target.view() {
            ValueView::Routine { name, package, .. } => {
                let (name, package) = (name.resolve(), package.resolve());
                let qualified = crate::qualified::qualified_text(&package, &name)
                    .as_str()
                    .to_string();
                let proto = self
                    .resolve_proto_function(&qualified)
                    .or_else(|| self.resolve_proto_function_with_alias(&name).map(|(_, def)| def));
                Some(Value::truth(proto.is_none_or(|def| {
                    super::dispatch_proto_rewrite::is_only_star_block(&def.body)
                })))
            }
            ValueView::Sub(data) => {
                let dispatcher = self
                    .routine_is_dispatcher(target)
                    .is_some_and(|v| v.truthy());
                // The proto's own body is `{*}` (the onlystar term, or the bare
                // `*` the registry's copy keeps), or empty.
                let only_star = data.body.is_empty()
                    || super::dispatch_proto_rewrite::is_only_star_block(&data.body)
                    || matches!(data.body.as_slice(),
                        [crate::ast::Stmt::Expr(e)] if e.is_onlystar_dispatch());
                Some(Value::truth(dispatcher && only_star))
            }
            _ => None,
        }
    }

    /// `Routine.multi`: whether the routine is one candidate of a multi. A
    /// `Routine` handle names the dispatcher, which is not.
    // Cost: O(1).
    pub(crate) fn routine_multi(&mut self, target: &Value) -> Option<Value> {
        match target.view() {
            ValueView::Routine { .. } => Some(Value::FALSE),
            ValueView::Sub(data) => Some(Value::truth(
                data.env.get("__mutsu_is_multi_candidate").is_some(),
            )),
            _ => None,
        }
    }

    /// `Routine.dispatcher`: the proto that dispatches a multi candidate. Raku
    /// generates one even when the source declares no `proto`, and
    /// `.dispatcher` returns it in either case: re-exporting `.dispatcher`
    /// under the routine's name (the `NativeLibs` custom-`EXPORT` idiom, which
    /// hands its importers `'&trait_mod:<is>' => $candidate.dispatcher`) must
    /// not narrow the multi to the one candidate that was introspected.
    ///
    /// A routine that is part of no multi has no dispatcher; Rakudo answers an
    /// NQPMu there, which is falsy, so `Nil` is the observable match
    /// (`&plain.dispatcher` is `?? !!`-tested by callers, not named).
    ///
    /// TODO: asking a dispatcher for its own `.dispatcher` should be `Nil` too.
    /// mutsu cannot yet tell `&mm` from `&mm.candidates[0]` -- neither carries
    /// the `__mutsu_is_multi_candidate` fact [`Self::routine_multi`] reads -- so
    /// it answers a handle to itself.
    // Cost: O(m + s + n), m = owner MRO length, s = family signature metadata,
    // n = qualified routine name bytes. Rakudo: O(1) -- see #14279.
    pub(crate) fn routine_dispatcher(&mut self, target: &Value) -> Option<Value> {
        match target.view() {
            ValueView::Routine { name, package, .. } => {
                if self.routine_name_is_multi(&package.resolve(), &name.resolve()) {
                    Some(Value::routine_parts(package, name, false))
                } else {
                    Some(target.clone())
                }
            }
            ValueView::Sub(data) => {
                if let Some((owner, name)) = data.routine_cell.method_family() {
                    return Some(self.classhow_lookup(&Value::package(owner), name.as_str())
                        .unwrap_or(Value::NIL));
                }
                let name = data.name.resolve();
                let qualified = crate::qualified::qualified_text(data.package.resolve(), &name)
                    .as_str()
                    .to_string();
                if self.has_multi_candidates(&name) {
                    // The dispatcher captures the candidates visible now, the
                    // way `&name` of a multi does (`sub_value_from_multi_candidates`).
                    // A by-name handle dangled once the candidates' scope ended:
                    // a `multi` declared inside `sub EXPORT` is lexical to it,
                    // so `'&trait_mod:<is>' => $t.dispatcher` reached the
                    // importer with no candidates at all (#11530).
                    let candidates = self.resolve_all_multi_candidates(&name);
                    let dispatcher = self.sub_value_from_multi_candidates(&name, candidates);
                    Some(self.dispatcher_containing(dispatcher, target))
                } else if self.resolve_proto_function(&qualified).is_some()
                    || self.resolve_proto_function_with_alias(&name).is_some()
                {
                    Some(Value::routine_parts(data.package, data.name, false))
                } else {
                    Some(Value::NIL)
                }
            }
            _ => None,
        }
    }

    /// `Routine.package`: the package the routine was declared in. A `&Pkg::name`
    /// handle's package is the one its name spells. A `Method` object built by
    /// `.^methods` for a multi's dispatcher records no package (Rakudo reports
    /// a stub one).
    // Cost: O(n), n = the handle's name length.
    pub(crate) fn routine_package(&self, target: &Value) -> Option<Value> {
        match target.view() {
            ValueView::Sub(data) => Some(Value::package(data.package)),
            ValueView::Routine { package, name, .. } => Some(Value::package(
                crate::qualified::package_parent(name)
                    .filter(|_| crate::qualified::is_qualified(name))
                    .unwrap_or(package),
            )),
            ValueView::Instance { .. } => Some(Value::NIL),
            _ => None,
        }
    }
}
