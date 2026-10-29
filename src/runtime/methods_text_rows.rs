//! The interpreter routines behind `Str`'s and `Cool`'s text rows that need
//! the interpreter (ADR-11276 §9.75, §9.76): `subst`, `IO`, `sprintf`, `printf`
//! and `split`.
//! The `method_table::str_interp` rows and the by-name arms of
//! `dispatch_method_by_name_1` both call these, so each has one
//! implementation.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// `.subst($matcher, $replacement?, *%options)`. A string (non-regex)
    /// pattern leaves the caller's `$/` alone, as in Rakudo; `dispatch_subst`
    /// publishes the match for `subst-mutate` and for a closure replacement, so
    /// it is restored. A regex argument may be a closure over its defining
    /// scope, installed around the substitution the way `~~` does.
    // Cost: O(n) in the haystack for a plain pattern, plus one replacement call
    // per match for a closure replacement.
    pub(crate) fn dispatch_subst_row(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let keep_slash = args
            .first()
            .is_some_and(|a| matches!(a.deref_container().view(), ValueView::Str(_)))
            && !matches!(
                args.get(1).map(Value::view),
                Some(ValueView::Sub(_))
                    | Some(ValueView::WeakSub(_))
                    | Some(ValueView::Routine { .. })
            );
        let saved = keep_slash.then(|| self.env.get("/").cloned());
        let result = self.with_regex_closure_scope(args.first().cloned(), |me| {
            me.dispatch_subst(target, &args)
        });
        if let Some(saved) = saved {
            match saved {
                Some(v) => {
                    self.env.insert("/".to_string(), v);
                }
                None => {
                    self.env.remove("/");
                }
            }
        }
        result
    }

    /// `Cool.IO`: the `IO::Path` the invocant's string names. It takes only the
    /// named adverbs `:CWD` and `:SPEC`, which it accepts and ignores: per Raku,
    /// `'.'.IO(:CWD($x))` does not forward `:CWD` to the path (unlike
    /// `IO::Path.new('.', :CWD($x))`), so the path keeps the current
    /// `$*CWD`/`$*SPEC`. `None` for any other argument and for a receiver whose
    /// own `.IO` answers (an IO handle, an instance with no `Cool` ancestry).
    // Cost: O(n), n = chars of the invocant's string form.
    pub(crate) fn dispatch_io_coerce(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let only_path_adverbs = args.iter().all(|a| {
            matches!(a.view(), ValueView::Pair(k, _) if k == "CWD" || k == "SPEC")
                || matches!(a.view(), ValueView::ValuePair(k, _)
                        if k.to_string_value() == "CWD" || k.to_string_value() == "SPEC")
        });
        if !only_path_adverbs {
            return None;
        }
        // IO handle objects have their own `.IO` (returning the path of
        // the open file); never coerce them via stringification.
        if matches!(target.view(), ValueView::Instance { class_name, .. }
            if matches!(class_name.resolve().as_str(),
                "IO::Handle" | "IO::Pipe" | "IO::CatHandle"))
        {
            return None;
        }
        // ADR-0051 P4: an `Instance` receiver whose ancestry does not
        // actually provide `.IO` (no `Cool` anywhere in its dispatch
        // chain) must not be answered by this generic interceptor --
        // it is receiver-class-blind by construction (unconditional
        // `target.to_string_value()`), which is exactly the leak the
        // ADR closes. `user_shadows_builtin`
        // already routes this call past this function entirely for a
        // plain (non-wildcard-handles) class; this is the direct
        // guard the ADR also calls for, so this interceptor stays
        // correct even if reached by a future call path that skips
        // those two gates.
        if matches!(target.view(), ValueView::Instance { .. })
            && !self.e2_native_method_exists(target, "IO")
        {
            return None;
        }
        // `.IO` on an IO::Path (sub)class type object returns the type
        // object itself, so `IO::Path::Unix === IO::Path::Unix.IO`.
        if let ValueView::Package(name) = target.view() {
            let n = name.resolve();
            if n == "IO::Path" || n.starts_with("IO::Path::") {
                return Some(Ok(target.clone()));
            }
            if let Some(err) = crate::runtime::utils::dateish_io_concreteness_error(&n) {
                return Some(Err(err));
            }
        }
        // Cool's `.IO` on an undefined invocant -- a type object
        // (`Str.IO`) or `Nil` -- is the `IO::Path` type object, not a
        // path named after the invocant's gist (`"(Str)".IO`) or `""`
        // (#9495).
        if matches!(target.view(), ValueView::Package(_) | ValueView::Nil) {
            return Some(Ok(Value::package(Symbol::intern("IO::Path"))));
        }
        let s = target.to_string_value();
        if s.contains('\0') {
            return Some(Err(RuntimeError::new(
                "X::IO::Null: Found null byte in pathname",
            )));
        }
        Some(Ok(self.make_io_path_instance(&s)))
    }

    /// `Cool.printf(*@args)`: `printf($format, @args)`, the same `Cool`
    /// convention `.sprintf` follows (documented in
    /// `Type/independent-routines.rakudoc`). With no argument the invocant is
    /// printed as a format with no directive. An `Instance` receiver is left to
    /// its own `printf` (`$*OUT.printf(...)`, an `IO::CatHandle`). `Cool.printf`
    /// has no `Junction:D` candidate -- a Junction *argument* is a directive
    /// error here, exactly as in Rakudo, so it must not autothread; a Junction
    /// *invocant* does autothread (it is the `Str(Cool)` format), which
    /// `call_function` handles.
    // Cost: O(n), n = chars of the formatted text (rendered, then written).
    pub(crate) fn dispatch_printf_row(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.is_empty()
            && !Self::is_io_cathandle(target)
            && !matches!(target.view(), ValueView::Junction { .. })
        {
            return Some(self.dispatch_printf(target));
        }
        if matches!(target.view(), ValueView::Instance { .. }) {
            return None;
        }
        let mut full = Vec::with_capacity(args.len() + 1);
        full.push(target.clone());
        full.extend(args.iter().cloned());
        if matches!(target.view(), ValueView::Junction { .. }) {
            return Some(self.call_function("printf", full));
        }
        Some((|| {
            let formatted = self.builtin_sprintf(&full, false)?;
            self.write_to_named_handle("$*OUT", &formatted.to_string_value(), false)?;
            Ok(Value::TRUE)
        })())
    }

    /// `Cool.sprintf(*@args)`: `sprintf($format, @args)`, through
    /// `builtin_sprintf` (the single source of truth), so a bare type object
    /// argument gets the same string-context warning and `""` coercion as the
    /// sub form. With no argument it is a no-directive passthrough. A Junction
    /// invocant is the `Str(Cool) $format`, which autothreads: `call_function`
    /// owns that threading.
    // Cost: O(n), n = chars of the formatted text.
    pub(crate) fn dispatch_sprintf_row(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let is_junction = matches!(target.view(), ValueView::Junction { .. });
        if args.is_empty() && !is_junction {
            return Some(self.dispatch_sprintf(target));
        }
        let mut full = Vec::with_capacity(args.len() + 1);
        full.push(target.clone());
        full.extend(args.iter().cloned());
        if is_junction {
            return Some(self.call_function("sprintf", full));
        }
        Some(self.builtin_sprintf(&full, false))
    }

    /// `.split($splitter, $limit?, *%adverbs)`: the receiver's string split by a
    /// string, a list of strings or a regex. A regex splitter may be a stored
    /// regex closed over its defining scope, installed around the split the way
    /// `~~` and `.match`/`.subst` do.
    // Cost: O(n) in the haystack, plus the splitter's own cost.
    pub(crate) fn dispatch_split_row(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // A named regex passed as a value (`.split(&Named)`) is a `Routine`;
        // split with its regex source, as `.subst` and `.match` do.
        let mut args = args;
        if let Some(first) = args.first()
            && let Some(p) = self.named_regex_routine_pattern(first)
        {
            args[0] = Value::regex(p);
        }
        let splitter = args.first().cloned();
        self.with_regex_closure_scope(splitter, |me| me.handle_split_method(target, args))
    }
}
