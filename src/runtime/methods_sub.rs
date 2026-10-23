use super::*;
use crate::symbol::Symbol;
use crate::value::ValueMap;
use crate::value::signature::{
    SubSignatureKey, cache_sub_signature, cached_sub_signature, extract_sig_info,
    make_signature_value, param_defs_to_sig_info,
};

/// Build a structured `X::Routine::Unwrap` error (`&f.unwrap($bad-handle)`).
pub(super) fn routine_unwrap_error(message: &str) -> RuntimeError {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.to_string()));
    let mut err = RuntimeError::new(message.to_string());
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::Routine::Unwrap"),
        attrs,
    )));
    err
}

/// Attributes for a `Routine::WrapHandle` returned by wrapping a method
/// CANDIDATE (`.^lookup(...).candidates[N].wrap(...)`, or the `Method`
/// instance path in `methods_instance_ops.rs`) — as opposed to a plain sub
/// wrap, which is keyed by `sub-id` instead. Carrying the owning class,
/// method name, and candidate index lets `.unwrap()`/`.restore()` find and
/// remove the right `Registry::method_wrap_chains` entry later (ADR-0019
/// E10's "unwrap method-wrap leak" fix — previously these attrs carried no
/// way to locate the entry at all, so it was never removed).
pub(super) fn method_wrap_handle_attrs(
    class_name: &str,
    method_name: &str,
    candidate_idx: usize,
    handle_id: u64,
    wrapped: &Value,
) -> ValueMap {
    let mut attrs = ValueMap::default();
    attrs.insert("wrap-class".to_string(), Value::str(class_name.to_string()));
    attrs.insert(
        "wrap-method".to_string(),
        Value::str(method_name.to_string()),
    );
    attrs.insert(
        "wrap-candidate-idx".to_string(),
        Value::int(candidate_idx as i64),
    );
    attrs.insert("handle-id".to_string(), Value::int(handle_id as i64));
    attrs.insert("wrapped-sub".to_string(), wrapped.clone());
    attrs
}

impl Interpreter {
    /// Dispatch methods on callable values (Routine, Sub, WeakSub).
    /// Returns Some(result) if handled, None to fall through to common dispatch.
    pub(super) fn dispatch_callable_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // `of`, `returns`, `arity`, `count`, `signature`, `line`, `file` and
        // `set_name` are `Code` rows (`method_table::code`, ADR-12523), and
        // `is_dispatcher`, `dispatcher`, `multi` and `package` are `Routine`
        // rows (`method_table::routine`, ADR-11276 §9.121).
        if let Some(answer) = crate::builtins::method_table::code::answer(self, target, method, args)
        {
            return Some(answer);
        }
        match target.view() {
            ValueView::Routine {
                name,
                package,
                is_regex,
                ..
            } => self.dispatch_routine_method(
                target,
                &name.resolve(),
                &package.resolve(),
                method,
                args.to_vec(),
                is_regex,
            ),
            ValueView::Sub(data) => {
                let data = data.clone();
                self.dispatch_sub_method(target, &data, method, args.to_vec())
            }
            ValueView::WeakSub(weak) => weak.upgrade().map(|strong| {
                self.call_method_with_values(Value::sub_value(strong), method, args.to_vec())
            }),
            _ => None,
        }
    }

    /// Dispatch methods on Routine (multi-dispatch handles).
    /// Returns Some(result) if handled, None to fall through.
    pub(super) fn dispatch_routine_method(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
        is_regex: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        // `readonly` is not Rakudo's; it answers the inverse of the `rw` row.
        if method == "readonly" && args.is_empty() {
            return self.routine_handle_rw(target, name, package, method, args, is_regex);
        }
        // `gist` and `raku` (`methods_code_render.rs`).
        if matches!(method, "raku" | "gist")
            && args.is_empty()
            && let Some(answer) = self.routine_handle_render(target, name, package, method)
        {
            return Some(answer);
        }
        // Cost: O(n), n = requested method-name length.
        if method == "can" {
            let method_name = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let can = matches!(
                method_name.as_str(),
                "assuming"
                    | "signature"
                    | "arity"
                    | "count"
                    | "candidates"
                    | "cando"
                    | "of"
                    | "name"
                    | "raku"
                    | "Str"
                    | "gist"
                    | "can"
                    | "dispatcher"
                    | "is_dispatcher"
                    | "multi"
                    | "package"
                    | "line"
                    | "file"
                    | "wrap"
                    | "unwrap"
                    | "rw"
                    | "readonly"
            );
            let methods = if can {
                vec![Value::routine_parts(
                    Symbol::intern("Code"),
                    Symbol::intern(&method_name),
                    false,
                )]
            } else {
                Vec::new()
            };
            return Some(Ok(Value::array(methods)));
        }
        None
    }

    /// `Code.arity` and `Code.count` (`method` is one of them): the answer for a
    /// `Sub`, a `&name` handle, a regex or a `WeakSub`'s referent. `None` for any
    /// other value.
    // Cost: O(p) for a routine with p parameters; O(c * p) for a multi
    // dispatcher with c candidates.
    pub(crate) fn code_arity_count(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        match target.view() {
            ValueView::Routine { name, package, .. } => {
                let target = target.clone();
                self.routine_arity_count(&target, name, package, method)
            }
            ValueView::Sub(data) => {
                let data = data.clone();
                self.sub_arity_count(&data, method)
            }
            ValueView::WeakSub(weak) => {
                let strong = weak.upgrade()?;
                self.code_arity_count(&Value::sub_value(strong), method)
            }
            ValueView::Regex(..) | ValueView::RegexWithAdverbs(..) => Some(Ok(
                self.regex_value_routine_introspection(target, method),
            )),
            _ => None,
        }
    }

    fn routine_arity_count(
        &mut self,
        target: &Value,
        name: Symbol,
        package: Symbol,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(sig) = self.dispatcher_signature(&package.resolve(), &name.resolve()) {
            return Some(Ok(Self::signature_arity_or_count(&sig, method)));
        }

        let (params, param_defs) = self.callable_signature(target);
        let defs = if !param_defs.is_empty() {
            param_defs
        } else {
            params
                .into_iter()
                .map(|name| ParamDef { spans: Default::default(), trait_meta: Default::default(),
                    type_capture: None,
                    name,
                    default: None,
                    multi_invocant: true,
                    required: true,
                    named: false,
                    named_alias: false,
                    slurpy: false,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                    block_param: false,
                    code: Default::default(),
                    trait_args: Vec::new(),
                })
                .collect()
        };
        let info = param_defs_to_sig_info(&defs, None);
        Some(Ok(if method == "arity" {
            Value::int(Self::signature_required_positional_count(&info))
        } else {
            Self::signature_count_value(&info)
        }))
    }

    fn sub_arity_count(
        &mut self,
        data: &crate::value::SubData,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // A signature bound to `$!signature` answers for the routine.
        if let Some(sig) = data.routine_cell.bound_signature() {
            return Some(Ok(Self::signature_arity_or_count(&sig, method)));
        }
        // A multi sub's dispatcher answers its proto's arity/count.
        if let Some(sig) = self.sub_dispatcher_signature(data) {
            return Some(Ok(Self::signature_arity_or_count(&sig, method)));
        }
        // Multi-dispatch dispatcher (a multi method's): this Sub's OWN param_defs are
        // empty (it is a synthesized dispatcher, not a declared body), so
        // reading `arity`/`count` off `data` directly always answered 0.
        // Mirrors the "signature"/"candidates"/"cando" handling just
        // above: try the live name-based candidates first (keeps working
        // if the name is still declared and picks up any candidate added
        // since capture), falling back to the candidates captured BY
        // VALUE at `&name` capture time — the only source left once the
        // name's own import scope has popped (see `resolve_code_var`,
        // accessors_resolve.rs).
        if let Some(ValueView::Str(disp_name)) =
            data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
        {
            let name_based =
                self.routine_candidate_subs(&data.package.resolve(), disp_name.as_str());
            if !name_based.is_empty()
                && let Some(result) = self.multi_candidate_arity_count(&name_based, method)
            {
                return Some(Ok(result));
            }
        }
        if let Some(ValueView::Array(cands, _)) = data
            .env
            .get("__mutsu_multi_dispatch_candidates")
            .map(Value::view)
            && let Some(result) = self.multi_candidate_arity_count(&cands, method)
        {
            return Some(Ok(result));
        }
        let sig = self.sub_signature_value(data);
        if let Some(info) = extract_sig_info(&sig) {
            return Some(Ok(if method == "arity" {
                Value::int(Self::signature_required_positional_count(&info))
            } else {
                Self::signature_count_value(&info)
            }));
        }
        Some(Ok(Value::int(0)))
    }

    /// `Code.signature`: the `Signature` of a `Sub`, a `&name` handle, a regex or
    /// a `WeakSub`'s referent. `None` for any other value.
    // Cost: O(p) for a routine with p parameters (cached after the first read of
    // a name-based handle); O(c * p) for a multi method's dispatcher with c
    // candidates.
    pub(crate) fn code_signature(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        match target.view() {
            ValueView::Routine { name, package, .. } => {
                let target = target.clone();
                self.routine_signature(&target, &name.resolve(), &package.resolve())
            }
            ValueView::Sub(data) => {
                let data = data.clone();
                self.sub_signature_of(&data)
            }
            ValueView::WeakSub(weak) => {
                let strong = weak.upgrade()?;
                self.code_signature(&Value::sub_value(strong))
            }
            ValueView::Regex(..) | ValueView::RegexWithAdverbs(..) => Some(Ok(
                self.regex_value_routine_introspection(target, "signature"),
            )),
            _ => None,
        }
    }

    fn routine_signature(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // A name with `multi` candidates is its dispatcher, whose
        // signature is the proto's (declared or generated).
        if let Some(sig) = self.dispatcher_signature(package, name) {
            return Some(Ok(sig));
        }
        let cache_key = SubSignatureKey::from_routine_handle(package, name);
        if let Some(cached) = cached_sub_signature(&cache_key) {
            return Some(Ok(cached));
        }
        let (params, param_defs) = self.callable_signature(target);
        let defs = if !param_defs.is_empty() {
            param_defs
        } else {
            params
                .into_iter()
                .map(|name| ParamDef { spans: Default::default(), trait_meta: Default::default(),
                    type_capture: None,
                    name,
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    named_alias: false,
                    slurpy: false,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                    block_param: false,
                    code: Default::default(),
                    trait_args: Vec::new(),
                })
                .collect()
        };
        let return_type = self.routine_return_spec_by_name(name);
        let info = param_defs_to_sig_info(&defs, return_type);
        let signature = make_signature_value(info, Some(&*self));
        cache_sub_signature(cache_key, signature.clone());
        Some(Ok(signature))
    }

    fn sub_signature_of(
        &mut self,
        data: &crate::value::SubData,
    ) -> Option<Result<Value, RuntimeError>> {
        // A multi sub's dispatcher answers its proto's signature (declared
        // or generated), never its candidates'.
        if let Some(sig) = self.sub_dispatcher_signature(data) {
            return Some(Ok(sig));
        }
        // A multi method's dispatcher: try name-based lookup first
        if let Some(ValueView::Str(disp_name)) =
            data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
        {
            let name_based =
                self.routine_candidate_subs(&data.package.resolve(), disp_name.as_str());
            if !name_based.is_empty() {
                let sigs: Vec<Value> = name_based
                    .iter()
                    .filter_map(|c| {
                        if let ValueView::Sub(cd) = c.view() {
                            Some(self.sub_signature_value(&cd))
                        } else {
                            None
                        }
                    })
                    .collect();
                if sigs.len() == 1 {
                    return Some(Ok(sigs.into_iter().next().unwrap()));
                }
                return Some(Ok(Value::junction(crate::value::JunctionKind::Any, sigs)));
            }
        }
        // Fall back to captured candidates
        if let Some(ValueView::Array(cands, _)) = data
            .env
            .get("__mutsu_multi_dispatch_candidates")
            .map(Value::view)
        {
            let sigs: Vec<Value> = cands
                .iter()
                .filter_map(|c| {
                    if let ValueView::Sub(cd) = c.view() {
                        Some(self.sub_signature_value(&cd))
                    } else {
                        None
                    }
                })
                .collect();
            if sigs.len() == 1 {
                return Some(Ok(sigs.into_iter().next().unwrap()));
            }
            return Some(Ok(Value::junction(crate::value::JunctionKind::Any, sigs)));
        }
        Some(Ok(self.sub_signature_value(data)))
    }

    /// `Code.line` and `Code.file` (`method` is one of them): the declaration
    /// location of a `Sub`, a `&name` handle or a `WeakSub`'s referent. A regex
    /// records none, and `None` is any other value.
    // Cost: O(1) for a routine carrying its own location; otherwise one registry
    // lookup by name.
    pub(crate) fn code_line_file(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        match target.view() {
            ValueView::Routine { name, package, .. } => {
                self.routine_line_file(&name.resolve(), &package.resolve(), method)
            }
            ValueView::Sub(data) => {
                let data = data.clone();
                self.sub_line_file(&data, method)
            }
            ValueView::WeakSub(weak) => {
                let strong = weak.upgrade()?;
                self.code_line_file(&Value::sub_value(strong), method)
            }
            // mutsu records no declaration location for a regex literal (a
            // constant in the pool) or a grammar `token`/`rule` body
            // (`Registry::token_defs` keeps the declaring file but no line).
            ValueView::Regex(..) | ValueView::RegexWithAdverbs(..) => Some(Ok(Value::NIL)),
            _ => None,
        }
    }

    fn routine_line_file(
        &mut self,
        name: &str,
        package: &str,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // A `Routine` value names a routine by (package, name) and carries
        // no body of its own — a proto/token reached by name, or a core
        // builtin/operator implemented in Rust. See `routine_decl_location`
        // for why the latter answers `Nil` rather than a synthesized
        // `SETTING::` path.
        let (line, file) = self.routine_decl_location(package, name);
        Some(Ok(if method == "line" {
            line.map(|l| Value::int(l as i64)).unwrap_or(Value::NIL)
        } else {
            file.map(|f| {
                let source_file = Symbol::intern(&f);
                Value::str(self.format_routine_file(f, Some(source_file)))
            })
            .unwrap_or(Value::NIL)
        }))
    }

    fn sub_line_file(
        &mut self,
        data: &crate::value::SubData,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        // A declared routine, closure or block carries its own declaration
        // location; a multi *dispatcher* (`&mm` for a `multi sub mm`) is
        // built by name with no compiled body behind it, so it answers from
        // its first candidate the way Rakudo does.
        let (line, file) = if data.source_line.is_some() || data.source_file.is_some() {
            (data.source_line, data.source_file.clone())
        } else {
            self.routine_decl_location(&data.package.resolve(), &data.name.resolve())
        };
        Some(Ok(if method == "line" {
            line.map(|l| Value::int(l as i64)).unwrap_or(Value::NIL)
        } else {
            file.map(|f| {
                let source_file = Symbol::intern(&f);
                Value::str(self.format_routine_file(f, Some(source_file)))
            })
            .unwrap_or(Value::NIL)
        }))
    }

    /// `Code.name`: the name of a `Sub`, a `&name` handle, a regex or a
    /// `WeakSub`'s referent, `""` when anonymous. `None` for any other value.
    // Cost: O(1) for a routine with its own name; O(n) to unqualify and format a
    // handle's name, n = its length.
    pub(crate) fn code_name_value(&self, target: &Value) -> Option<Value> {
        match target.view() {
            ValueView::Routine { name, .. } => Some(Value::str(super::methods_instance_ops::format_operator_name(
                crate::qualified::unqualified_part(name).as_str(),
            ))),
            // The name lives with the routine's `$!do` (#11462).
            ValueView::Sub(data) => Some(Value::str(super::methods_instance_ops::format_operator_name(
                self.code_name(&data).as_str(),
            ))),
            ValueView::WeakSub(weak) => {
                let strong = weak.upgrade()?;
                self.code_name_value(&Value::sub_value(strong))
            }
            // A regex is a `Code`, and an anonymous one's name is "" until
            // `set_name` gives it one.
            ValueView::Regex(..) | ValueView::RegexWithAdverbs(..) => Some(Value::str(
                target
                    .regex_name()
                    .map(|name| name.resolve())
                    .unwrap_or_default(),
            )),
            _ => None,
        }
    }

    /// `Code.Str`: the routine's name (`""` for an anonymous one), with the
    /// resumable warning Rakudo's `Code.Str` emits, `<Type> object coerced to
    /// string (please use .gist or .raku to do that)`. A regex says `Regex`
    /// (`regex_str_coercion`). `None` for a value that is not a code object.
    // Cost: O(n) to format the name and the warning, n = chars of the name.
    pub(crate) fn code_str(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        if let Some(answer) = self.regex_str_coercion(target) {
            return Some(answer);
        }
        // A `Method`/`Submethod` (`.^lookup`, `.^methods`) is an `Instance`
        // whose own `name` method answers the name.
        let name = if let ValueView::Instance { class_name, .. } = target.view()
            && matches!(class_name.as_str(), "Method" | "Submethod")
        {
            match self.try_compiled_method_or_interpret(target.clone(), "name", Vec::new()) {
                Ok(name) => name,
                Err(e) => return Some(Err(e)),
            }
        } else {
            self.code_name_value(target)?
        };
        let name = if name.to_string_value() == "<anon>" {
            Value::str_from("")
        } else {
            name
        };
        let type_name = crate::value::types::what_type_name(target);
        Some(self.raise_resumable_warning(
            &format!("{type_name} object coerced to string (please use .gist or .raku to do that)"),
            name,
        ))
    }

    /// Dispatch methods on Sub.
    /// Returns Some(result) if handled, None to fall through.
    pub(super) fn dispatch_sub_method(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if method == "nextwith" {
            // Tail-style dispatch: call target with caller frame and return from current frame.
            let saved_env = self.env.clone();
            let saved_line = self.cur_source_line;
            if let Some(parent_env) = self.caller_env_stack.last().cloned() {
                self.env = parent_env;
                // Frame elimination must also apply to the line tracking: the
                // target's $?CALLER::LINE is the *eliminated* frame's callsite
                // (recorded in the frame entry at push), not the nextwith line.
                if let Some(entry) = self.callframe_stack.last() {
                    self.cur_source_line = entry.line;
                }
            }
            let call_result = self.call_sub_value(target.clone(), args);
            self.env = saved_env;
            self.cur_source_line = saved_line;
            let value = match call_result {
                Ok(v) => v,
                Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                Err(e) => return Some(Err(e)),
            };
            return Some(Err(RuntimeError {
                return_value: Some(value),
                ..RuntimeError::new("")
            }));
        }
        // `gist` and `raku` (`methods_code_render.rs`).
        if matches!(method, "raku" | "gist")
            && args.is_empty()
            && let Some(answer) = self.sub_render(target, data, method)
        {
            return Some(answer);
        }
        if method == "readonly" && args.is_empty() {
            return Some(Ok(Value::truth(!data.is_rw)));
        }
        if method == "callwith" {
            // &sub.callwith(args) — call the sub directly with provided args
            return Some(self.call_sub_value(target.clone(), args));
        }
        // Cost: O(n), n = requested method-name length.
        if method == "can" {
            let method_name = args
                .first()
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let can = matches!(
                method_name.as_str(),
                "assuming"
                    | "signature"
                    | "arity"
                    | "count"
                    | "candidates"
                    | "cando"
                    | "of"
                    | "name"
                    | "raku"
                    | "Str"
                    | "gist"
                    | "can"
                    | "wrap"
                    | "unwrap"
                    | "callwith"
                    | "is_dispatcher"
                    | "multi"
                    | "package"
                    | "line"
                    | "file"
            );
            let methods = if can {
                vec![Value::routine_parts(
                    Symbol::intern("Code"),
                    Symbol::intern(&method_name),
                    false,
                )]
            } else {
                Vec::new()
            };
            return Some(Ok(Value::array(methods)));
        }
        None
    }

    /// Extract wrap handle ID from a WrapHandle instance.
    pub(super) fn extract_wrap_handle_id(&self, handle: &Value) -> Option<u64> {
        match handle.view() {
            ValueView::Instance { attributes, .. } => {
                if let Some(ValueView::Int(id)) =
                    attributes.as_map().get("handle-id").map(Value::view)
                {
                    Some(id as u64)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Clean up wrap_sub_names and wrap_name_to_sub when a sub's wrap chain becomes empty.
    pub(crate) fn cleanup_wrap_name_entries(&mut self, sub_id: u64) {
        if let Some(name) = self.dispatch.wrap_sub_names.remove(&sub_id) {
            self.dispatch.wrap_name_to_sub.remove(&name);
        }
    }

    /// Drop named wrap state when a new declaration replaces the routine.
    ///
    /// `&name` creates a fresh `Sub` value, while named calls find the wrap
    /// chain through these name-keyed tables. A later lexical declaration of
    /// the same name (notably one compiled by `EVAL`) therefore has to retire
    /// the old chain before it becomes the current registry entry; otherwise
    /// calls to the new routine can still dispatch the old wrapper.
    pub(crate) fn clear_wrap_chains_for_name(&mut self, name: &str) {
        let sub_ids: Vec<u64> = self
            .dispatch
            .wrap_sub_names
            .iter()
            .filter_map(|(sub_id, sub_name)| (sub_name == name).then_some(*sub_id))
            .collect();
        if sub_ids.is_empty() {
            return;
        }
        let wrap_chains = crate::runtime::cow_table_mut(&mut self.dispatch.wrap_chains);
        for sub_id in sub_ids {
            wrap_chains.remove(&sub_id);
            self.dispatch.wrap_sub_names.remove(&sub_id);
        }
        self.dispatch.wrap_name_to_sub.remove(name);
        crate::runtime::cow_table_mut(&mut self.dispatch.wrap_callable_ids).remove(name);
        self.invalidate_fn_resolution();
    }
}

/// Rename a code object in place, so every alias of it reports the new name.
/// The one body behind `Code.set_name` and `nqp::setcodename`. Returns `false`
/// when `target` is not a code object.
// Cost: O(n), n = chars of the new name (interned).
pub(crate) fn rename_code_object(target: &Value, name: &str) -> bool {
    let ValueView::Sub(gc) = target.view() else {
        return false;
    };
    // SAFETY: a single write of a `Copy` field of the shared node; no borrow
    // into this `SubData` is dereferenced across it. Code objects are not
    // structurally mutated from another thread.
    unsafe { crate::value::gc_contents_mut(&gc).name = Symbol::intern(name) };
    // Every other value of the same routine (`&foo` is rebuilt on each read)
    // answers the new name too (#11844).
    gc.routine_cell.rename(Symbol::intern(name));
    true
}

/// The `Routine.prec` key behind `.precedence` / `.associative`.
pub(super) fn prec_field_of(method: &str) -> Option<&'static str> {
    match method {
        "precedence" => Some("prec"),
        "associative" => Some("assoc"),
        _ => None,
    }
}
