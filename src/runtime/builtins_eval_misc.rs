use super::*;
use crate::value::ValueView;

impl Interpreter {
    pub(super) fn builtin_make(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = if args.len() > 1 {
            Value::slip_arc(std::sync::Arc::new(args.to_vec()))
        } else {
            args.first().cloned().unwrap_or(Value::NIL)
        };
        self.env.insert("made".to_string(), value.clone());
        self.action_made = Some(value.clone());
        Ok(value)
    }

    pub(super) fn builtin_made(&self) -> Result<Value, RuntimeError> {
        Ok(self.env.get("made").cloned().unwrap_or(Value::NIL))
    }

    pub(crate) fn builtin_callframe(
        &self,
        args: &[Value],
        default_depth: usize,
    ) -> Result<Value, RuntimeError> {
        let mut depth = default_depth;
        let mut callsite_line: Option<i64> = None;
        let mut nblocks: usize = 0;
        for arg in args {
            match arg.view() {
                ValueView::Int(i) if i >= 0 => depth = i as usize,
                ValueView::Num(f) if f >= 0.0 => depth = f as usize,
                ValueView::Pair(k, v) if k == "__callframe_line" => {
                    if let ValueView::Int(line) = v.view() {
                        callsite_line = Some(line);
                    }
                }
                ValueView::Pair(k, v) if k == "__callframe_blocks" => {
                    if let ValueView::Int(n) = v.view() {
                        nblocks = n.max(0) as usize;
                    }
                }
                _ => {}
            }
        }
        if let Some(frame) = self.callframe_value(depth, callsite_line, nblocks) {
            return Ok(frame);
        }
        Ok(Value::NIL)
    }

    /// Implementation of `caller()` function.
    pub(crate) fn builtin_caller(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut type_filter: Option<String> = None;
        let mut skip: usize = 0;
        let mut callsite_line: Option<i64> = None;
        for arg in args {
            match arg.view() {
                ValueView::Pair(k, v) if k == "__callframe_line" => {
                    if let ValueView::Int(line) = v.view() {
                        callsite_line = Some(line);
                    }
                }
                ValueView::Pair(k, v) if k == "skip" => {
                    skip = match v.view() {
                        ValueView::Int(i) => i as usize,
                        _ => v.to_string_value().parse::<usize>().unwrap_or(0),
                    };
                }
                ValueView::Package(name) => {
                    type_filter = Some(name.resolve());
                }
                ValueView::Str(s) => {
                    type_filter = Some(s.to_string());
                }
                ValueView::Mixin(inner, _) => {
                    type_filter = Some(inner.to_string_value());
                }
                _ => {}
            }
        }

        let stack = &self.routine_stack;

        // Step 1: Find the current routine by walking from the top,
        // skipping block frames (is_block == true).
        // Then go one more frame up to find the caller of that routine.
        let mut caller_start_idx: Option<usize> = None;
        let mut found_routine = false;
        for i in (0..stack.len()).rev() {
            let frame = &stack[i];
            if !frame.is_block {
                if found_routine {
                    // This is the caller of the current routine
                    caller_start_idx = Some(i);
                    break;
                }
                // This is the current routine; skip it
                found_routine = true;
            }
        }

        // If no caller found in routine_stack, fall back to callframe
        if caller_start_idx.is_none() && type_filter.is_none() && skip == 0 {
            if let Some(frame) = self.callframe_value(1, callsite_line, 0) {
                return Ok(frame);
            }
            return Ok(Value::package(Symbol::intern("Mu")));
        }

        let start_idx = match caller_start_idx {
            Some(idx) => idx,
            None => return Ok(Value::package(Symbol::intern("Mu"))),
        };

        if let Some(ref filter) = type_filter {
            let is_any = filter == "Any" || filter == "Mu";
            let mut found = 0;
            for i in (0..=start_idx).rev() {
                let frame = &stack[i];
                if frame.is_block {
                    continue;
                }
                let matches = if is_any {
                    true
                } else if filter == "Method" {
                    frame.is_method
                } else if filter == "Sub" || filter == "SubRoutine" {
                    !frame.is_method
                } else {
                    false
                };
                if matches {
                    if found == skip {
                        // Get callsite line from the frame above this one
                        let callsite = self.get_callsite_line_for_frame(i);
                        return Ok(self.build_caller_frame(frame, callsite));
                    }
                    found += 1;
                }
            }
            return Ok(Value::package(Symbol::intern("Mu")));
        }

        // No type filter: skip N non-block frames from the caller position
        let mut skipped = 0;
        for i in (0..=start_idx).rev() {
            let frame = &stack[i];
            if frame.is_block {
                continue;
            }
            if skipped == skip {
                let callsite = self.get_callsite_line_for_frame(i);
                return Ok(self.build_caller_frame(frame, callsite));
            }
            skipped += 1;
        }
        Ok(Value::package(Symbol::intern("Mu")))
    }

    /// Get the call-site line for a routine frame at the given index.
    /// This is the line in the frame's code where it called the next frame.
    /// We look at the frame one index above (the callee) for this info.
    fn get_callsite_line_for_frame(&self, frame_idx: usize) -> Option<i64> {
        let stack = &self.routine_stack;
        // Walk up from frame_idx+1 to find the next non-block frame
        for f in stack.iter().skip(frame_idx + 1) {
            if !f.is_block {
                return f.line.map(|l| l as i64);
            }
        }
        // If no callee found, try the topmost frame
        if let Some(f) = stack.get(frame_idx + 1) {
            return f.line.map(|l| l as i64);
        }
        None
    }

    fn build_caller_frame(&self, frame: &RoutineFrame, callsite_line: Option<i64>) -> Value {
        let mut attrs = HashMap::new();
        let pkg: &str = if frame.package.is_empty() || frame.package == "GLOBAL" {
            "Main"
        } else {
            frame.package.as_str()
        };
        let subname = format!("&{}::{}", pkg, frame.name);
        attrs.insert("subname".to_string(), Value::str(subname));
        attrs.insert("package".to_string(), Value::str(pkg.to_string()));

        let file = frame
            .file
            .map(|s| s.resolve())
            .or_else(|| self.env.get("?FILE").map(|v| v.to_string_value()))
            .unwrap_or_default();
        attrs.insert("file".to_string(), Value::str(file));

        let line = callsite_line
            .or_else(|| frame.line.map(|l| l as i64))
            .unwrap_or(0);
        attrs.insert("line".to_string(), Value::int(line));

        let subtype = if frame.is_method {
            "Method"
        } else {
            "SubRoutine"
        };
        attrs.insert("subtype".to_string(), Value::str(subtype.to_string()));

        // Search the block_stack for a Sub matching this frame's name. A
        // routine ever composed with a role (`.^mixin(Role)`, or a trait
        // handler's `$r does Role`) is a `Mixin` wrapping its `Sub` here, not
        // a bare `Sub` — see `Interpreter::materialize_routine_mixins` — so
        // look through that wrapper the same way.
        let sub_val = self
            .block_stack
            .iter()
            .rev()
            .find(|v| {
                let sd = match v.view() {
                    ValueView::Sub(sd) => Some(sd),
                    ValueView::Mixin(inner, _) => match inner.as_ref().view() {
                        ValueView::Sub(sd) => Some(sd),
                        _ => None,
                    },
                    _ => None,
                };
                let Some(sd) = sd else { return false };
                sd.name == frame.name
                    && (sd.package == frame.package
                        || (frame.package == "GLOBAL" && sd.package.is_empty())
                        || (sd.package == "GLOBAL" && frame.package.is_empty()))
            })
            .cloned()
            .or_else(|| self.env.get(&format!("&{}", frame.name)).cloned())
            .or_else(|| self.env.get_sym(frame.name).cloned())
            .unwrap_or(Value::NIL);
        attrs.insert("sub".to_string(), sub_val);
        attrs.insert("inline".to_string(), Value::FALSE);
        attrs.insert("annotations".to_string(), self.build_annotations(&attrs));
        // caller() returns Control::Caller type per spec
        Value::make_instance(Symbol::intern("Control::Caller"), attrs)
    }

    /// The synthesized name of the next `EVAL` compilation unit: `EVAL_0`,
    /// `EVAL_1`, ... The counter is per process and is consumed only when a
    /// name is actually synthesized, so an `EVAL ..., :filename` call does not
    /// advance it (rakudo behaves the same way).
    fn next_eval_unit_name() -> String {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static EVAL_UNIT_COUNTER: AtomicUsize = AtomicUsize::new(0);
        format!("EVAL_{}", EVAL_UNIT_COUNTER.fetch_add(1, Ordering::Relaxed))
    }

    /// Absolutify a compilation-unit name for `$?FILE`, which is always an
    /// absolute path even when the unit's own name is relative (`EVAL_0`,
    /// or a relative `:filename`). An already-absolute name is left alone.
    fn absolutify_unit_name(&self, name: &str) -> String {
        let path = std::path::Path::new(name);
        if path.is_absolute() {
            return name.to_string();
        }
        // Resolve against `$*CWD` rather than the process CWD so a script that
        // has changed `$*CWD` still names its EVAL units where it thinks it is.
        let cwd = self
            .env
            .get("*CWD")
            .map(|v| v.to_string_value())
            .filter(|s| !s.is_empty())
            .or_else(|| {
                std::env::current_dir()
                    .ok()
                    .map(|p| p.to_string_lossy().into_owned())
            });
        match cwd {
            Some(cwd) => std::path::Path::new(&cwd)
                .join(path)
                .to_string_lossy()
                .into_owned(),
            None => name.to_string(),
        }
    }

    pub(super) fn builtin_evalfile(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("EVALFILE requires a filename"))?;
        let code = fs::read_to_string(&path)
            .map_err(|err| RuntimeError::new(format!("Failed to read {}: {}", path, err)))?;
        let saved_file = self.env.get("?FILE").cloned();
        self.env.insert("?FILE".to_string(), Value::str(path));
        let result = self.eval_eval_string(&code);
        if let Some(prev) = saved_file {
            self.env.insert("?FILE".to_string(), prev);
        } else {
            self.env.remove("?FILE");
        }
        result
    }

    pub(super) fn builtin_eval(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // EVAL of a RakuAST node (ADR-0011 Phase 5): lower it to the internal AST
        // and run it through the existing evaluator — no new execution engine.
        if let Some(first_arg) = Self::positional_value(args, 0)
            && let ValueView::RakuAst(node) = first_arg.view()
        {
            let stmts = crate::rakuast::lower(node)?;
            return self.eval_block_value(&stmts);
        }
        // EVAL only accepts strings (and Buf), not blocks
        if let Some(first_arg) = Self::positional_value(args, 0) {
            match first_arg.view() {
                ValueView::Sub(_) | ValueView::Routine { .. } | ValueView::WeakSub(_) => {
                    return Err(RuntimeError::new(
                        "EVAL() requires a string or Buf argument, not a Block",
                    ));
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    // Buf argument: decode as UTF-8
                    if let Some(bytes) = crate::value::value_buf::buf_bytes(&attributes) {
                        let code = String::from_utf8_lossy(&bytes).to_string();
                        return self.eval_eval_string(&code);
                    }
                }
                _ => {}
            }
        }
        let code = Self::positional_string(args, 0);
        if Self::has_invalid_anonymous_rw_trait(&code) {
            return Err(RuntimeError::new(
                "X::Trait::Invalid: trait 'rw' is not valid on anonymous parameter",
            ));
        }
        if let Some(lang) = Self::named_value(args, "lang") {
            let lang = lang.to_string_value();
            if !lang.eq_ignore_ascii_case("raku") && !lang.eq_ignore_ascii_case("perl6") {
                let message = format!("No compiler available for language '{}'", lang);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("lang".to_string(), Value::str(lang));
                attrs.insert("message".to_string(), Value::str(message.clone()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Eval::NoSuchLang"),
                    attrs,
                );
                let mut err = RuntimeError::new(message);
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }
        // `EVAL $code, context => $ctx` compiles the string as if it stood at
        // `$ctx`'s frame, so a package the snippet declares is named after the
        // caller and not after whichever module called EVAL. Without this a
        // `throws-like 'class Foo { ... }', X::...` written against the real
        // `Test` module reports `Test::Foo`.
        let context_arg = Self::named_value(args, "context");
        let saved_package = context_arg
            .as_ref()
            .and_then(Self::eval_context_package)
            .map(|pkg| {
                let saved = self.current_package();
                self.set_current_package(pkg);
                saved
            });
        // ADR-0037 §2.3: classify what the snippet's `return` should do from
        // `$ctx`'s stamped routine identity (§2.2), once, here at EVAL entry
        // -- not at the `return` itself, since the snippet runs synchronously
        // inside this call so no frame below it can disappear in between.
        // `None` (no `context` argument at all) leaves
        // `compile_block_value_opts`'s ambient classification untouched.
        let saved_context_routine = context_arg.as_ref().map(|ctx| {
            let state = self.classify_eval_context_routine(ctx);
            self.pending_eval_context_routine.replace(state)
        });
        let check_only = Self::named_value(args, "check").is_some_and(|v| v.truthy());
        // Each EVAL is its own compilation unit with its own name: the explicit
        // `:filename` argument, or a synthesized `EVAL_<N>` (N a per-process
        // counter consumed only when a name is actually synthesized). The name
        // is what `Code.file` reports, while `$?FILE` reports it absolutified
        // against `$*CWD` -- the same as-invoked / absolute split the mainline
        // compilation unit already has.
        let unit_name = Self::named_value(args, "filename")
            .map(|v| v.to_string_value())
            .unwrap_or_else(Self::next_eval_unit_name);
        let saved_env_file = self.env.get("?FILE").cloned();
        // Record which unit this EVAL was compiled inside. `EVAL` compiles in
        // the caller's lexical scope, so an operator declared in the enclosing
        // unit is in scope for the EVAL'd code too -- and an operator declared
        // BY the EVAL'd code is scoped to the EVAL unit alone. Operator
        // visibility (`Interpreter::user_infix_override`) walks this chain.
        let unit_sym = Symbol::intern(&unit_name);
        crate::runtime::note_eval_unit_parent(unit_sym, self.current_unit);
        let saved_unit = std::mem::replace(&mut self.current_unit, unit_sym);
        self.env
            .insert("?FILE".to_string(), Value::str(unit_name.clone()));
        let saved_source_file =
            crate::parser::set_parser_source_file(Some(self.absolutify_unit_name(&unit_name)));
        let result = if check_only {
            self.eval_eval_string_check_only(&code)
        } else {
            self.eval_eval_string(&code)
        };
        crate::parser::set_parser_source_file(saved_source_file);
        self.current_unit = saved_unit;
        match saved_env_file {
            Some(prev) => {
                self.env.insert("?FILE".to_string(), prev);
            }
            None => {
                self.env.remove("?FILE");
            }
        }
        if let Some(saved) = saved_context_routine {
            self.pending_eval_context_routine = saved;
        }
        if let Some(saved) = saved_package {
            self.set_current_package(saved);
        }
        result
    }

    /// ADR-0037 §2.3: classify `ctx`'s (an `EVAL ..., context => $ctx`
    /// argument) routine identity into what the EVAL unit's `return` should
    /// do. `Mainline` both when `ctx` carries no routine identity at all
    /// (`eval_context_routine` returns `None` — either `ctx` is not a stamped
    /// pseudo-stash, or `CALLER::` named the mainline) and when it does but
    /// no live frame matches it (checked against `Dead` below only when a key
    /// IS present, so this covers the "not a frame-derived context" case,
    /// e.g. `context => SomePackage`, exactly like a bare package name has no
    /// dynamic-scope identity of its own).
    ///
    /// Liveness is a walk of `routine_stack` comparing `package::name`
    /// against the recorded key -- sound only because ADR-0037 Slice 1 made
    /// every sub dispatch path push a routine frame; a re-entrant same-named
    /// routine is indistinguishable by that key and resolves to the
    /// innermost live frame (searched from the top of the stack down),
    /// matching `return`'s own lexical semantics.
    ///
    /// ADR-0037 Slice 4: when a live frame matches, also resolve its
    /// registration clone id (`registration_clone_id`, keyed the same way
    /// `RuntimeError::return_target_callable_id` is) so the caller can bake
    /// it onto the compiled EVAL unit's `Return` and target that frame
    /// specifically. `Live(None)` for a live frame with no resolvable id
    /// (e.g. an anonymous routine, which never registers
    /// `__mutsu_callable_id`) -- falls back to the pre-Slice-4 behavior of
    /// the first routine boundary catching the signal.
    fn classify_eval_context_routine(&self, ctx: &Value) -> EvalContextRoutineState {
        let Some(key) = Self::eval_context_routine(ctx) else {
            return EvalContextRoutineState::Mainline;
        };
        let live_frame = self
            .routine_stack
            .iter()
            .rev()
            .find(|f| !f.is_block && format!("{}::{}", f.package, f.name) == key);
        match live_frame {
            Some(frame) => {
                let target_id =
                    self.registration_clone_id(&frame.package.resolve(), &frame.name.resolve());
                EvalContextRoutineState::Live(target_id)
            }
            None => EvalContextRoutineState::Dead,
        }
    }

    pub(super) fn builtin_dd(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let arg_sources = self.pending_call_arg_sources.clone().unwrap_or_default();
        for (i, val) in args.iter().enumerate() {
            let source_name = arg_sources.get(i).and_then(|entry| entry.as_deref());
            // A user-class instance's `.raku` needs the class registry to collect
            // its public attributes, which the static `raku_value` cannot reach
            // (it would render `F()`); dispatch the instance method instead.
            let value_repr = if matches!(val.view(), ValueView::Instance { .. }) {
                self.call_method_with_values(val.clone(), "raku", vec![])
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|_| crate::builtins::methods_0arg::raku_repr::raku_value(val))
            } else {
                crate::builtins::methods_0arg::raku_repr::raku_value(val)
            };
            let repr = Self::dd_format_with_repr(val, source_name, value_repr);
            self.emit_stderr(&format!("{}\n", repr));
        }
        Ok(args.first().cloned().unwrap_or(Value::NIL))
    }

    /// Format a value for `dd` output (Raku-style debug representation).
    ///
    /// The value part is the value's `.raku` representation. When the argument
    /// is a plain variable (e.g. `dd %h`), Raku prefixes it with the runtime
    /// type and the variable name: `Hash %h = {:a(1)}`. Literals and complex
    /// expressions render as just the value.
    /// Format a `dd` line from a precomputed value representation. The caller
    /// supplies the value repr — an interpreter-dispatched `.raku` for a
    /// user-class instance (whose attribute list needs the class registry), or
    /// the static `raku_value` otherwise. When the argument is a plain variable
    /// (`dd %h`), Raku prefixes it with the runtime type and name.
    fn dd_format_with_repr(val: &Value, source_name: Option<&str>, value_repr: String) -> String {
        match source_name {
            Some(name) if Self::dd_is_plain_var(name) => {
                let ty = crate::runtime::utils::value_type_name(val);
                format!("{} {} = {}", ty, name, value_repr)
            }
            _ => value_repr,
        }
    }

    /// A `dd` argument source counts as a named variable only when it is a bare
    /// sigil + identifier (`$x`, `@a`, `%h`, `&c`) — not an index/expression.
    fn dd_is_plain_var(name: &str) -> bool {
        let mut chars = name.chars();
        match chars.next() {
            Some('$') | Some('@') | Some('%') | Some('&') => {}
            _ => return false,
        }
        let rest = &name[1..];
        !rest.is_empty()
            && rest
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == ':')
    }
}
