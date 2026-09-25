//! `nqp::getcomp("Raku")` and the context protocol a persistent REPL is built
//! on (ADR-0122).
//!
//! Rakudo's own `REPL`, and the ecosystem sandboxes copied from it (CodeUnit,
//! Jupyter::Kernel, Text::CodeProcessing), all keep one session alive the
//! same way:
//!
//! ```raku
//! my $*CTXSAVE  := self;          # an object with a `ctxsave` method
//! my $*MAIN_CTX := $context;      # what the previous line left behind
//! my $value := $compiler.eval($code, :outer_ctx($context));
//! $context := $*MAIN_CTX;         # set by ctxsave, run from the unit's mainline
//!
//! method ctxsave { $*MAIN_CTX := nqp::ctxcaller(nqp::ctx); $*CTXSAVE := 0 }
//! ```
//!
//! mutsu has no first-class lexical frame, so a context here is an
//! [`EvalContext`] held in a per-interpreter side table and named from Raku by
//! a `BOOTContext` instance carrying its index. What it captures is what the
//! next line has to see:
//!
//! * `env` — the unit's final environment, taken right after its mainline ran
//!   and before `EVAL`'s rollback drops its `my` lexicals
//!   (`parse_and_eval_with_operators`). Seeding the next unit's env from it is
//!   what makes `my $a = 42` then `$a` work.
//! * `routines` / `infix_ops` — the lexical subs (operators included) the unit
//!   declared. `EVAL` rolls the routine registry back when it returns, so a
//!   later line would lose them; they are re-installed around each unit that
//!   runs in the context, and rolled back after it again, so they never leak
//!   into the host program.
//!
//! Classes and other package-scoped declarations need nothing: `EVAL` already
//! keeps those, as rakudo does.

use super::*;
use crate::runtime::repl_compiler_prelude::REPL_COMPILER_PRELUDE;

/// Class name of the Raku-visible handle on an [`EvalContext`] (rakudo's
/// `nqp::ctx` answers a `BOOTContext` too).
const CONTEXT_CLASS: &str = "BOOTContext";
/// Attribute carrying the side-table index. Hidden by the `__mutsu_` prefix.
const CONTEXT_ID_ATTR: &str = "__mutsu_ctx_id";

/// One lexical context a later evaluation can be compiled inside.
#[derive(Clone, Default)]
pub(crate) struct EvalContext {
    env: Env,
    /// Where `nqp::ctxcaller` of this context leads, when known.
    caller: Option<usize>,
    routines: Vec<(Symbol, Arc<FunctionDef>)>,
    infix_ops: Vec<(String, HashSet<Symbol>)>,
}

/// What one compilation unit left behind, recorded by the EVAL pipeline when
/// [`ReplCompilerState::capture_request`] is armed.
pub(crate) struct UnitCapture {
    pub(crate) env: Env,
    pub(crate) routines: Vec<(Symbol, Arc<FunctionDef>)>,
    pub(crate) infix_ops: Vec<(String, HashSet<Symbol>)>,
}

/// The routine table and the user-operator table, as one unit started.
type CaptureBase = (
    Arc<crate::runtime::function_table::FunctionTable>,
    Arc<HashMap<String, HashSet<Symbol>>>,
);

#[derive(Default)]
pub(crate) struct ReplCompilerState {
    // TODO: contexts are never freed. A REPL creates one per line, which is
    // fine for an interactive session, but a long-running sandbox grows
    // without bound; a weak handle (or GC-traced context values) would let
    // an unreachable context go.
    contexts: Vec<EvalContext>,
    /// Armed by `compiler_eval` for exactly the next compilation unit: the
    /// EVAL pipeline takes it just before running the unit's mainline, so an
    /// `EVAL` nested inside that mainline never sees it.
    pub(crate) capture_request: bool,
    /// `block_scope_depth` of the armed unit's body while it runs.
    capture_depth: Option<usize>,
    pub(crate) captured: Option<UnitCapture>,
    /// The routine registry and user-operator table as they stood when the
    /// unit started, which the capture diffs against.
    capture_base: Option<CaptureBase>,
    /// While `$*CTXSAVE.ctxsave` runs: the unit context it should hand back,
    /// and the `caller_env_stack` depth the call was made from.
    ctxsave_unit: Option<(usize, usize)>,
    compiler: Option<Value>,
}

impl Interpreter {
    fn new_eval_context(&mut self, ctx: EvalContext) -> Value {
        let id = self.repl_compiler.contexts.len();
        self.repl_compiler.contexts.push(ctx);
        let mut attrs = HashMap::new();
        attrs.insert(CONTEXT_ID_ATTR.to_string(), Value::int(id as i64));
        Value::make_instance(Symbol::intern(CONTEXT_CLASS), attrs)
    }

    fn eval_context_id(value: &Value) -> Option<usize> {
        match value.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == CONTEXT_CLASS => attributes
                .as_map()
                .get(CONTEXT_ID_ATTR)
                .map(|v| crate::runtime::to_int(v) as usize),
            _ => None,
        }
    }

    /// Register the `Perl6::Compiler` and `REPL` classes, once.
    pub(crate) fn ensure_repl_compiler_prelude(&mut self) -> Result<(), RuntimeError> {
        if self.has_class("Perl6::Compiler") {
            return Ok(());
        }
        let (stmts, _) = crate::parse_dispatch::parse_source(REPL_COMPILER_PRELUDE)?;
        let saved_package = self.current_package();
        self.set_current_package("GLOBAL".to_string());
        let result = self.eval_block_value(&stmts);
        self.set_current_package(saved_package);
        result.map(|_| ())
    }

    /// `nqp::getcomp($name)`: the compiler object for `Raku`, or null (as
    /// `nqp::null` answers it) for any other language.
    // Cost: O(1) after the first call, which parses and registers the prelude once.
    pub(crate) fn nqp_getcomp(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let name = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // rakudo registers its compiler under `Raku` alone; the old `perl6`
        // name that sandboxes still try as a fallback answers null there too.
        if name != "Raku" {
            return Ok(Value::NIL);
        }
        if let Some(compiler) = &self.repl_compiler.compiler {
            return Ok(compiler.clone());
        }
        self.ensure_repl_compiler_prelude()?;
        let compiler = Value::make_instance(Symbol::intern("Perl6::Compiler"), HashMap::new());
        self.repl_compiler.compiler = Some(compiler.clone());
        Ok(compiler)
    }

    /// `nqp::ctx()`: the current frame's context.
    // Cost: O(v), v = variables visible in the current frame (the env is flattened into the snapshot).
    pub(crate) fn nqp_ctx(&mut self) -> Value {
        let depth = self.caller_env_stack_depth();
        // Inside `$*CTXSAVE.ctxsave` (called by `compiler_eval` below), the
        // caller of this frame is the unit that just ran, not whatever Rust
        // frame made the call.
        let caller = match self.repl_compiler.ctxsave_unit {
            // A method call made from Rust may or may not push a caller
            // frame, depending on the dispatch path it takes.
            Some((unit, call_depth)) if depth == call_depth || depth == call_depth + 1 => {
                Some(unit)
            }
            _ => None,
        };
        let caller = caller.or_else(|| {
            let env = self.caller_env_stack.last()?.flattened();
            let id = self.repl_compiler.contexts.len();
            self.repl_compiler.contexts.push(EvalContext {
                env,
                ..EvalContext::default()
            });
            Some(id)
        });
        let env = self.env.flattened();
        self.new_eval_context(EvalContext {
            env,
            caller,
            ..EvalContext::default()
        })
    }

    /// `nqp::ctxcaller($ctx)`: the context `$ctx`'s frame was called from, or
    /// `Mu` when that is not known.
    // Cost: O(1).
    pub(crate) fn nqp_ctxcaller(&mut self, args: &[Value]) -> Value {
        let caller = args
            .first()
            .and_then(Self::eval_context_id)
            .and_then(|id| self.repl_compiler.contexts.get(id))
            .and_then(|ctx| ctx.caller);
        match caller {
            Some(id) => {
                let mut attrs = HashMap::new();
                attrs.insert(CONTEXT_ID_ATTR.to_string(), Value::int(id as i64));
                Value::make_instance(Symbol::intern(CONTEXT_CLASS), attrs)
            }
            None => Value::package(Symbol::intern("Mu")),
        }
    }

    /// `nqp::ctxlexpad($ctx)`: the context's user lexicals, as a hash keyed by
    /// their sigiled names.
    // Cost: O(v), v = variables in the context.
    pub(crate) fn nqp_ctxlexpad(&mut self, args: &[Value]) -> Value {
        let mut pad = crate::value::ValueMap::default();
        if let Some(ctx) = args
            .first()
            .and_then(Self::eval_context_id)
            .and_then(|id| self.repl_compiler.contexts.get(id))
        {
            for (key, value) in ctx.env.iter() {
                key.with_str(|name| {
                    if !crate::env::is_plain_user_lexical(name) {
                        return;
                    }
                    let spelled = if name.starts_with(['@', '%', '&']) {
                        name.to_string()
                    } else {
                        format!("${name}")
                    };
                    pad.insert(spelled, value.clone());
                });
            }
        }
        Value::hash(pad)
    }

    /// `__mutsu_compiler_eval($code, $outer_ctx, $ctxsave)` — the Rust half of
    /// `Perl6::Compiler.eval` (see the module docs for the protocol).
    // Cost: O(1) to recognize the name; a match costs what `compiler_eval` states.
    pub(crate) fn try_repl_compiler_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_compiler_eval" {
            return None;
        }
        let code = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let outer = args.get(1).and_then(Self::eval_context_id);
        let saver = args.get(2).cloned().unwrap_or(Value::NIL);
        Some(self.compiler_eval(&code, outer, saver))
    }

    // Cost: O(v + r), v = variables and r = routines in the outer context (seeded into the unit, then captured from it), plus the unit's own compile and run.
    fn compiler_eval(
        &mut self,
        code: &str,
        outer: Option<usize>,
        saver: Value,
    ) -> Result<Value, RuntimeError> {
        let outer_ctx = outer.and_then(|id| self.repl_compiler.contexts.get(id).cloned());
        // Without an outer context the unit sees the setting only, as in
        // rakudo -- not the lexicals of whichever method called `.eval`.
        let unit_env = outer_ctx
            .as_ref()
            .map(|ctx| ctx.env.clone())
            .unwrap_or_default();
        let saved_env = std::mem::replace(&mut self.env, unit_env);
        let registry_before = self.snapshot_routine_registry();
        if let Some(ctx) = &outer_ctx {
            self.install_context_routines(ctx);
        }
        let base = (
            Arc::clone(&self.registry().functions),
            Arc::clone(&self.user_declared_infix_ops),
        );
        let saved_base = self.repl_compiler.capture_base.replace(base);
        self.repl_compiler.capture_request = true;
        self.repl_compiler.captured = None;
        // The unit is a fresh compilation unit's mainline, whatever Raku
        // method happened to call `.eval`: its subs are the unit's own, not
        // closures local to that method.
        let saved_routine = self
            .pending_eval_context_routine
            .replace(EvalContextRoutineState::Mainline);
        let result = self.builtin_eval(&[Value::str(code.to_string())]);
        self.pending_eval_context_routine = saved_routine;
        self.repl_compiler.capture_request = false;
        self.repl_compiler.capture_base = saved_base;
        let captured = self.repl_compiler.captured.take();
        self.restore_routine_registry(registry_before);
        self.env = saved_env;
        let value = result?;

        let Some(captured) = captured else {
            return Ok(value);
        };
        let mut routines = outer_ctx
            .as_ref()
            .map(|ctx| ctx.routines.clone())
            .unwrap_or_default();
        for (key, def) in captured.routines {
            routines.retain(|(k, _)| *k != key);
            routines.push((key, def));
        }
        let mut infix_ops = outer_ctx
            .as_ref()
            .map(|ctx| ctx.infix_ops.clone())
            .unwrap_or_default();
        infix_ops.extend(captured.infix_ops);
        let unit = self.repl_compiler.contexts.len();
        self.repl_compiler.contexts.push(EvalContext {
            env: captured.env,
            caller: None,
            routines,
            infix_ops,
        });

        // rakudo's mainline ends with `$*CTXSAVE.ctxsave` when the caller
        // armed one; that is how the unit's context gets out.
        if crate::runtime::types::value_is_defined(&saver)
            && self
                .call_method_with_values(saver.clone(), "can", vec![Value::str_from("ctxsave")])
                .is_ok_and(|can| can.truthy())
        {
            let depth = self.caller_env_stack_depth();
            let saved_unit = self.repl_compiler.ctxsave_unit.replace((unit, depth));
            let saved = self.call_method_with_values(saver, "ctxsave", Vec::new());
            self.repl_compiler.ctxsave_unit = saved_unit;
            saved?;
        }
        Ok(value)
    }

    /// Put an outer context's lexical routines back in scope for one unit.
    fn install_context_routines(&mut self, ctx: &EvalContext) {
        if !ctx.routines.is_empty() {
            let keys: Vec<Symbol> = ctx.routines.iter().map(|(k, _)| *k).collect();
            {
                let mut registry = self.registry_mut();
                let functions = registry.functions_mut();
                for (key, def) in &ctx.routines {
                    functions.insert(*key, def.clone());
                }
            }
            self.invalidate_fn_resolution_for_keys(keys);
        }
        if !ctx.infix_ops.is_empty() {
            let ops = std::sync::Arc::make_mut(&mut self.user_declared_infix_ops);
            for (op, units) in &ctx.infix_ops {
                ops.entry(op.clone())
                    .or_default()
                    .extend(units.iter().copied());
            }
        }
    }

    /// Called by the EVAL pipeline right before a unit's mainline runs:
    /// consumes an armed capture request and marks the block depth the unit's
    /// own body will run at, so [`Self::capture_eval_unit_scope`] fires for
    /// this unit only and not for an `EVAL` nested inside it.
    pub(crate) fn begin_unit_capture(&mut self) {
        if std::mem::take(&mut self.repl_compiler.capture_request) {
            self.repl_compiler.capture_depth = Some(self.block_scope_depth);
        }
    }

    /// Called after the unit's mainline returned.
    pub(crate) fn end_unit_capture(&mut self) {
        self.repl_compiler.capture_depth = None;
    }

    /// Called by `eval_block_value_inner` when a body's scope is about to be
    /// unwound -- its lexical subs rolled out of the registry, its `&name`
    /// bindings dropped. For the unit `compiler_eval` armed, that is the moment
    /// rakudo's mainline calls `ctxsave`, and what is still in scope is the
    /// unit's context: its env and the routines it added since `compiler_eval`
    /// took [`ReplCompilerState::capture_base`].
    pub(crate) fn capture_eval_unit_scope(&mut self, is_eval_unit: bool) {
        if !is_eval_unit || self.repl_compiler.capture_depth != Some(self.block_scope_depth) {
            return;
        }
        self.repl_compiler.capture_depth = None;
        let Some((before, infix_before)) = self.repl_compiler.capture_base.clone() else {
            return;
        };
        let routines: Vec<(Symbol, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(key, def)| before.get(*key).is_none_or(|old| !Arc::ptr_eq(old, def)))
            .map(|(key, def)| (*key, def.clone()))
            .collect();
        let infix_ops: Vec<(String, HashSet<Symbol>)> = self
            .user_declared_infix_ops
            .iter()
            .filter(|(op, units)| infix_before.get(*op) != Some(*units))
            .map(|(op, units)| (op.clone(), units.clone()))
            .collect();
        // The unit's own topic, match and error variables (and the EVAL
        // bookkeeping keys) belong to that unit's run, not to the context a
        // later unit is compiled in: a stale `$_` there was read back as the
        // next unit's value.
        let mut env = self.env.flattened();
        env.retain(|key, _| {
            key.with_str(|k| !matches!(k, "_" | "/" | "!" | "=pod" | "?FILE" | "__mutsu_in_eval"))
        });
        self.repl_compiler.captured = Some(UnitCapture {
            env,
            routines,
            infix_ops,
        });
    }
}
