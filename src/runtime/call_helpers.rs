use super::*;
use crate::value::ValueView;

const TEST_CALLSITE_LINE_KEY: &str = "__mutsu_test_callsite_line";

impl Interpreter {
    pub(crate) fn set_pending_call_arg_sources(&mut self, sources: Option<Vec<Option<String>>>) {
        self.pending_call_arg_sources = sources;
    }

    pub(crate) fn take_pending_call_arg_sources(&mut self) -> Option<Vec<Option<String>>> {
        self.pending_call_arg_sources.take()
    }

    /// Non-consuming peek at the pending call arg-source names, used by the
    /// method dispatch gate to decide whether a container argument shares the
    /// caller's container with a plain scalar `$` param (Slice 2d).
    pub(crate) fn pending_call_arg_sources(&self) -> Option<&Vec<Option<String>>> {
        self.pending_call_arg_sources.as_ref()
    }

    /// The caller's variable name a bare block's implicit `$_` may ALIAS for
    /// this call — `Interpreter::pending_call_topic_source`. `None` for every
    /// shape that is not a lone positional argument naming a plain scalar
    /// lexical, because those are the only ones raku binds the topic raw to.
    ///
    /// A block takes exactly one topic, so a multi-argument call has no topic
    /// to alias; a `@`/`%`/`&` source is a container in its own right (the
    /// topic would alias the whole array, not an element); `_` is the caller's
    /// OWN topic, which aliasing would make self-referential; and the `key=src`
    /// spelling is a named argument, which never becomes the topic.
    pub(crate) fn topic_alias_source(
        args: &[Value],
        arg_sources: Option<&Vec<Option<String>>>,
    ) -> Option<String> {
        let [arg] = args else {
            return None;
        };
        if arg.unwrap_varref().is_string_pair_value() {
            return None;
        }
        let name = arg_sources?.first()?.as_ref()?;
        if name == "_" || name.contains('=') {
            return None;
        }
        name.as_bytes()
            .first()
            .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
            .then(|| name.clone())
    }

    /// A method wrapper (`&m.wrap(-> \SELF, |c { ... })`) is invoked with the
    /// invocant PREPENDED to the method's arguments, but the pending
    /// call-site arg-source names were recorded by the call opcode for the
    /// method's arguments only. Without a shift they are off by one against
    /// the wrapper's signature, and a sigilless/`is raw`/`is rw` parameter —
    /// which re-reads its value from the named source variable rather than
    /// from the argument slot — binds the wrong value: `$obj.m($x, 2)` bound
    /// the wrapper's `\SELF` to `$x` instead of `$obj` (OO::Monitors' lock
    /// wrapper then read `$!MONITR-lock` off a Bool).
    pub(crate) fn shift_arg_sources_for_wrap_invocant(&mut self) {
        if let Some(sources) = self.pending_call_arg_sources.as_mut() {
            sources.insert(0, None);
        }
    }

    pub(crate) fn exec_call_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match self.call_function(name, args.clone()) {
            Ok(v) => Ok(v),
            Err(e)
                if e.message
                    .contains("Unknown function (call_function fallback disabled):") =>
            {
                self.exec_call(name, args)
            }
            Err(e) => Err(e),
        }
    }

    /// The `OpCode::ExecCallPairs` carrier arm.
    ///
    /// The opcode handler sanitizes the argument list at its entry (so its
    /// compiled/native probes see the real arguments, not one carrying the
    /// parser-injected `__mutsu_test_callsite_line` pair) and can hand over the
    /// routine it already resolved there. See
    /// [`Interpreter::exec_call_sanitized`] for what `pre_resolved` may carry.
    pub(crate) fn exec_call_pairs_values_sanitized(
        &mut self,
        name: &str,
        args: Vec<Value>,
        callsite_line: Option<i64>,
        pre_resolved: Option<std::sync::Arc<crate::ast::FunctionDef>>,
    ) -> Result<Value, RuntimeError> {
        // For EVAL, route through call_function to handle named args like :check.
        if name == "EVAL" {
            return match self.call_function(name, args.clone()) {
                Ok(v) => Ok(v),
                Err(e)
                    if e.message
                        .contains("Unknown function (call_function fallback disabled):") =>
                {
                    self.exec_call_sanitized(name, args, callsite_line, pre_resolved)
                }
                Err(e) => Err(e),
            };
        }
        self.exec_call_sanitized(name, args, callsite_line, pre_resolved)
    }

    pub(crate) fn is_callsite_line_marker(arg: &Value) -> bool {
        match arg.view() {
            ValueView::Pair(key, _) => key == TEST_CALLSITE_LINE_KEY,
            ValueView::ValuePair(key, _) => {
                matches!(key.view(), ValueView::Str(name) if name.as_str() == TEST_CALLSITE_LINE_KEY)
            }
            _ => false,
        }
    }

    pub(crate) fn peek_callsite_line(args: &[Value]) -> Option<i64> {
        for arg in args {
            if let Some(line) = Self::callsite_line_of_view(&arg.view()) {
                return Some(line);
            }
        }
        None
    }

    /// Per-value half of `peek_callsite_line`: the callsite line carried by one
    /// (already-decoded) arg view, `None` when the value is not the marker.
    /// Takes the view so a caller that already matched on it (e.g. the fused
    /// junction+marker scan on the light-call hit path, J4d) doesn't decode the
    /// nanbox twice.
    pub(crate) fn callsite_line_of_view(view: &ValueView) -> Option<i64> {
        let value = match view {
            ValueView::Pair(key, value) if *key == TEST_CALLSITE_LINE_KEY => value,
            ValueView::ValuePair(key, value) => match key.view() {
                ValueView::Str(name) if name.as_str() == TEST_CALLSITE_LINE_KEY => value,
                _ => return None,
            },
            _ => return None,
        };
        match value.view() {
            ValueView::Int(i) => Some(i),
            ValueView::BigInt(i) => i.to_string().parse::<i64>().ok(),
            ValueView::Num(n) => Some(n as i64),
            ValueView::Str(s) => s.parse::<i64>().ok(),
            _ => None,
        }
    }

    /// Owned-`Vec` variant of [`Self::sanitize_call_args`]: when no callsite-line
    /// marker is present (the common case) the caller's `Vec` is returned as-is,
    /// skipping the per-arg clone and the rebuilt `Vec` allocation.
    pub(crate) fn sanitize_call_args_owned(&self, args: Vec<Value>) -> (Vec<Value>, Option<i64>) {
        if args.iter().any(Self::is_callsite_line_marker) {
            return self.sanitize_call_args(&args);
        }
        (args, None)
    }

    pub(crate) fn sanitize_call_args(&self, args: &[Value]) -> (Vec<Value>, Option<i64>) {
        let mut out = Vec::with_capacity(args.len());
        let mut callsite_line = None;
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) => {
                    if key == TEST_CALLSITE_LINE_KEY {
                        callsite_line = match value.view() {
                            ValueView::Int(i) => Some(i),
                            ValueView::BigInt(i) => i.to_string().parse::<i64>().ok(),
                            ValueView::Num(n) => Some(n as i64),
                            ValueView::Str(s) => s.parse::<i64>().ok(),
                            _ => None,
                        };
                        continue;
                    }
                    out.push(arg.clone());
                }
                ValueView::ValuePair(key, value) => {
                    if let ValueView::Str(name) = key.view()
                        && name.as_str() == TEST_CALLSITE_LINE_KEY
                    {
                        callsite_line = match value.view() {
                            ValueView::Int(i) => Some(i),
                            ValueView::BigInt(i) => i.to_string().parse::<i64>().ok(),
                            ValueView::Num(n) => Some(n as i64),
                            ValueView::Str(s) => s.parse::<i64>().ok(),
                            _ => None,
                        };
                        continue;
                    }
                    // ValuePair is kept as-is (positional pair, e.g. from (:a(3)))
                    out.push(arg.clone());
                }
                _ => out.push(arg.clone()),
            }
        }
        (out, callsite_line)
    }

    pub(crate) fn pending_callsite_line(&self) -> Option<i64> {
        self.test_pending_callsite_line
    }

    pub(crate) fn set_pending_callsite_line(&mut self, line: Option<i64>) {
        self.test_pending_callsite_line = line;
    }

    pub(crate) fn inject_pending_callsite_line(&mut self) {
        if let Some(line) = self.test_pending_callsite_line {
            self.cur_source_line = line;
        }
    }

    pub(crate) fn push_test_assertion_context(&mut self, is_test_assertion: bool) -> bool {
        if !is_test_assertion {
            return false;
        }
        let line = self
            .test_assertion_line_stack
            .last()
            .copied()
            .or(self.test_pending_callsite_line)
            .unwrap_or(1);
        self.test_assertion_line_stack.push(line);
        true
    }

    pub(crate) fn pop_test_assertion_context(&mut self, pushed: bool) {
        if pushed {
            self.test_assertion_line_stack.pop();
        }
    }

    /// Current depth of `test_assertion_line_stack`, for panic-unwind
    /// recovery (`Interpreter::recover_call_frames_after_panic`). Pushed by
    /// `push_test_assertion_context`, popped by `pop_test_assertion_context`
    /// -- a stack separate from `call_frames`, so a panic caught mid-call
    /// leaves it holding every entry pushed since the boundary too. See
    /// `todo/deep/panic-unwind-leaks-side-channel-call-state.md`.
    pub(crate) fn test_assertion_line_stack_depth(&self) -> usize {
        self.test_assertion_line_stack.len()
    }

    /// Truncate `test_assertion_line_stack` back to `depth` (see
    /// [`Self::test_assertion_line_stack_depth`]).
    pub(crate) fn truncate_test_assertion_line_stack(&mut self, depth: usize) {
        self.test_assertion_line_stack
            .truncate(depth.min(self.test_assertion_line_stack.len()));
    }

    pub(crate) fn routine_is_test_assertion_by_name(&mut self, name: &str, args: &[Value]) -> bool {
        // Monotonic negative filter: unless some `is test-assertion` routine was
        // ever registered under this bare name, skip the full name resolution
        // (which clones an `Arc<FunctionDef>`'s AST per call — it profiled as
        // the single hottest allocation source in a tight compiled-call loop).
        if !crate::runtime::registration_sub::test_assertion_name_possible(name) {
            return false;
        }
        self.resolve_function_with_alias(name, args)
            .map(|def| def.is_test_assertion)
            .unwrap_or(false)
    }

    pub(super) fn positional_values(args: &[Value]) -> Vec<&Value> {
        args.iter()
            .filter(|v| !matches!(v.view(), ValueView::Pair(_, _)))
            .collect()
    }

    pub(super) fn positional_value(args: &[Value], index: usize) -> Option<&Value> {
        let mut count = 0;
        for arg in args {
            if !matches!(arg.view(), ValueView::Pair(_, _)) {
                if count == index {
                    return Some(arg);
                }
                count += 1;
            }
        }
        None
    }

    pub(super) fn positional_value_required<'a>(
        args: &'a [Value],
        index: usize,
        message: &str,
    ) -> Result<&'a Value, RuntimeError> {
        Self::positional_value(args, index).ok_or_else(|| RuntimeError::new(message.to_string()))
    }

    pub(super) fn positional_string(args: &[Value], index: usize) -> String {
        Self::positional_value(args, index)
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    }

    pub(super) fn named_bool(args: &[Value], name: &str) -> bool {
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view()
                && key == name
            {
                return value.truthy();
            }
        }
        false
    }

    pub(super) fn named_value(args: &[Value], name: &str) -> Option<Value> {
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view()
                && key == name
            {
                return Some(value.clone());
            }
        }
        None
    }

    /// Convert a Failure value into an Err (simulating sink-context behavior).
    /// In Raku, a Failure in sink context throws its wrapped exception.
    /// This is used by throws-like to detect Failures returned by code blocks.
    pub(crate) fn sink_failure_to_error(val: Value) -> Result<Value, RuntimeError> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = val.view()
            && class_name == "Failure"
            && let Some(exception) = attributes.as_map().get("exception")
        {
            let message = if let ValueView::Instance {
                attributes: ex_attrs,
                ..
            } = exception.view()
            {
                ex_attrs
                    .as_map()
                    .get("message")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default()
            } else {
                exception.to_string_value()
            };
            let mut err = RuntimeError::new(message);
            err.exception = Some(Box::new(exception.clone()));
            return Err(err);
        }
        Ok(val)
    }
}
