use super::main_args::{CandidateDispatch, ParsedMainArgs};
use super::*;
use std::collections::HashMap;

impl Interpreter {
    /// The `RUN-MAIN(&main, $mainline, :$in-as-argsfiles)` core sub. Gives a
    /// program complete control over `MAIN` handling. Reconstructs Rakudo's
    /// dispatch order:
    ///
    /// 1. If a `MAIN_HELPER` sub is in scope and neither new-interface hook
    ///    (`ARGS-TO-CAPTURE` / `GENERATE-USAGE`) is, use the old interface: call
    ///    `MAIN_HELPER` and return.
    /// 2. Otherwise build a `Capture` from `@*ARGS` — via a user-provided
    ///    `ARGS-TO-CAPTURE` if present, else the default arg parser — and try to
    ///    dispatch `&main` with it.
    /// 3. On a failed dispatch, generate a usage message (`GENERATE-USAGE`, else
    ///    the old `USAGE`, else the default), then exit via `&*EXIT` with code 0
    ///    when help was requested (a truthy `help` named arg) or 2 otherwise.
    ///
    /// The dynamic `&*ARGS-TO-CAPTURE` / `&*GENERATE-USAGE` are installed as
    /// callable `Sub` values so a user override can introspect them, and
    /// `&*EXIT` (which the caller may override) is honoured on exit.
    pub(crate) fn builtin_run_main(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Program manages MAIN itself: suppress mutsu's implicit end-of-program
        // dispatch so MAIN is not run a second time.
        self.explicit_run_main = true;
        let main = args.first().cloned().unwrap_or(Value::NIL);
        let mainline = args.get(1).cloned().unwrap_or(Value::NIL);

        let has_args_to_capture = self.resolve_function("ARGS-TO-CAPTURE").is_some();
        let has_generate_usage = self.resolve_function("GENERATE-USAGE").is_some();
        let has_main_helper = self.resolve_function("MAIN_HELPER").is_some();
        let has_usage = self.resolve_function("USAGE").is_some();

        // Install the default `&*ARGS-TO-CAPTURE` / `&*GENERATE-USAGE` dynamics
        // (unless the caller already set them) as real `Sub` values so
        // `&*ARGS-TO-CAPTURE ~~ Sub` holds.
        self.install_default_main_dynamics();

        // Old MAIN_HELPER interface: only when no new-interface hook is present.
        if has_main_helper && !has_args_to_capture && !has_generate_usage {
            // Rakudo calls the old helper with a single positional; its value is
            // irrelevant (both `($a = 0)` and `($, $a = 0)` bind one arg).
            self.call_function("MAIN_HELPER", vec![mainline])?;
            return Ok(Value::NIL);
        }

        let args_val = self
            .env
            .get("@*ARGS")
            .cloned()
            .unwrap_or_else(|| Value::array(Vec::new()));
        let raw_values = Self::value_to_list(&args_val);

        // Build the dispatch Capture.
        let capture = if has_args_to_capture {
            // Pass `@*ARGS` itself (an Array) so a user ARGS-TO-CAPTURE sees the
            // same value `@*ARGS.Array` yields.
            self.call_function("ARGS-TO-CAPTURE", vec![main.clone(), args_val])?
        } else {
            self.default_args_to_capture(&raw_values)
        };
        let (positional, named_pairs) = Self::capture_parts(&capture);
        let parsed = ParsedMainArgs {
            positional: positional.clone(),
            named: named_pairs.clone(),
        };
        let help_requested = named_pairs.iter().any(|(k, v)| k == "help" && v.truthy());

        // Try to dispatch &main with the capture.
        let sub_main_opts = self.read_sub_main_opts();
        let candidates = self.collect_main_candidates();
        let mut matched = false;
        for candidate in &candidates {
            match self.try_dispatch_candidate(candidate, &parsed, &sub_main_opts) {
                CandidateDispatch::Called => {
                    matched = true;
                    break;
                }
                CandidateDispatch::NoMatch => continue,
                CandidateDispatch::Error(e) => return Err(e),
            }
        }
        if matched {
            return Ok(Value::NIL);
        }

        // Failed dispatch: generate usage and exit.
        let usage_text = self.generate_usage_from_candidates(&candidates);
        self.env
            .insert("$*USAGE".to_string(), Value::str(usage_text.clone()));
        self.env
            .insert("*USAGE".to_string(), Value::str(usage_text.clone()));
        self.mark_readonly("$*USAGE");
        self.mark_readonly("*USAGE");

        if has_generate_usage {
            let mut ga = vec![main.clone()];
            ga.extend(positional.iter().cloned());
            for (k, v) in &named_pairs {
                ga.push(Value::pair(k.clone(), v.clone()));
            }
            let generated = self.call_function("GENERATE-USAGE", ga)?;
            self.emit_stderr(&format!("{}\n", generated.to_string_value()));
        } else if has_usage {
            self.call_function("USAGE", vec![])?;
        } else {
            self.emit_stderr(&format!("Usage:\n{}\n", usage_text));
        }

        let exit_code = if help_requested { 0 } else { 2 };
        self.run_main_exit(exit_code)?;
        Ok(Value::NIL)
    }

    /// Extract the positional list and named pairs (sorted by key) from a
    /// `Capture` value.
    fn capture_parts(capture: &Value) -> (Vec<Value>, Vec<(String, Value)>) {
        match capture.view() {
            ValueView::Capture { positional, named } => {
                let mut named_pairs: Vec<(String, Value)> =
                    named.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                named_pairs.sort_by(|a, b| a.0.cmp(&b.0));
                (positional.to_vec(), named_pairs)
            }
            _ => (Vec::new(), Vec::new()),
        }
    }

    /// The default `ARGS-TO-CAPTURE`: parse raw `@*ARGS` values into a `Capture`.
    /// Each argument value is `val()`-coerced to an allomorph (so `--n=42`
    /// yields `IntStr` `<42>`, matching Rakudo), and a repeated named option
    /// collects its values into an `Array`.
    pub(super) fn default_args_to_capture(&mut self, raw_values: &[Value]) -> Value {
        let sub_main_opts = self.read_sub_main_opts();
        // Determine bool/value-taking named options from the MAIN candidates so
        // `--verbose value` is parsed correctly for a typed signature; for a
        // slurpy `*%_` MAIN this is empty and every unknown named without `=`
        // becomes a boolean flag.
        let candidates = self.collect_main_candidates();
        let named_info: Vec<_> = candidates
            .iter()
            .flat_map(Self::extract_named_param_info)
            .collect::<Vec<_>>();
        let raw_strings: Vec<String> = raw_values.iter().map(|v| v.to_string_value()).collect();
        let parsed = match Self::parse_cli_args(&raw_strings, &named_info, &sub_main_opts) {
            Ok(p) => p,
            Err(_) => return Value::capture(Vec::new(), HashMap::new()),
        };
        let positional: Vec<Value> = parsed.positional.iter().map(Self::val_coerce).collect();
        // Collect named args, merging duplicates into an Array (Rakudo's CLI
        // processing turns repeated `--n=x --n=y` into `n => [x, y]`).
        let mut named: HashMap<String, Value> = HashMap::new();
        for (k, v) in &parsed.named {
            let coerced = Self::val_coerce(v);
            if let Some(existing) = named.get_mut(k) {
                if let ValueView::Array(arc, _) = existing.view() {
                    let mut items = arc.as_slice().to_vec();
                    items.push(coerced);
                    *existing = Value::real_array(items);
                } else {
                    // Rakudo collects a repeated `--n=x --n=y` into an `Array`
                    // (`[x, y]`), not a `List`, so `is-deeply` against `[...]`
                    // matches.
                    *existing = Value::real_array(vec![existing.clone(), coerced]);
                }
            } else {
                named.insert(k.clone(), coerced);
            }
        }
        Value::capture(positional, named)
    }

    /// Apply `val()` allomorph coercion to a parsed CLI value. A Bool (from a
    /// bare `--flag`) is left as-is; a string is run through `val`.
    fn val_coerce(v: &Value) -> Value {
        match v.view() {
            ValueView::Bool(_) => v.clone(),
            _ => crate::runtime::builtins_collection::builtin_val(std::slice::from_ref(v)),
        }
    }

    /// Install default `&*ARGS-TO-CAPTURE` / `&*GENERATE-USAGE` dynamics as
    /// `Sub` values, unless the caller already provided them.
    fn install_default_main_dynamics(&mut self) {
        if self.env.get("&*ARGS-TO-CAPTURE").is_none() {
            let sub = self.make_stub_sub("ARGS-TO-CAPTURE");
            self.env.insert("&*ARGS-TO-CAPTURE".to_string(), sub);
        }
        if self.env.get("&*GENERATE-USAGE").is_none() {
            let sub = self.make_stub_sub("GENERATE-USAGE");
            self.env.insert("&*GENERATE-USAGE".to_string(), sub);
        }
    }

    /// Build a minimal named `Sub` value. Used for the default main dynamics so
    /// they satisfy `~~ Sub` and can be introspected.
    // TODO: give these bodies that delegate to the real default arg parser /
    // usage generator so a user hook can call `&*ARGS-TO-CAPTURE(...)` to reuse
    // the default; not yet exercised by any test.
    fn make_stub_sub(&self, name: &str) -> Value {
        Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
            package: crate::symbol::Symbol::intern("GLOBAL"),
            name: crate::symbol::Symbol::intern(name),
            params: Vec::new(),
            param_defs: Vec::new(),
            body: Vec::new(),
            is_rw: false,
            is_raw: false,
            env: Env::new(),
            assumed_positional: Vec::new(),
            assumed_named: HashMap::new(),
            id: crate::value::next_instance_id(),
            empty_sig: false,
            is_bare_block: false,
            compiled_code: None,
            deprecated_message: None,
            source_line: None,
            source_file: None,
            owned_captures: Vec::new(),
            authoritative_captures: Vec::new(),
            upvalues: Vec::new(),
        }))
    }

    /// Perform the exit at the end of a failed `RUN-MAIN` dispatch. Honours a
    /// caller-installed `&*EXIT` dynamic (used by tests to intercept the exit);
    /// otherwise sets the process exit code.
    fn run_main_exit(&mut self, code: i64) -> Result<(), RuntimeError> {
        if let Some(exit_fn) = self.env.get("&*EXIT").cloned()
            && matches!(
                exit_fn.view(),
                ValueView::Sub(_) | ValueView::Routine { .. } | ValueView::WeakSub(_)
            )
        {
            self.call_sub_value(exit_fn, vec![Value::int(code)], false)?;
            return Ok(());
        }
        self.exit_code = code;
        Ok(())
    }
}
