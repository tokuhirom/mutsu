//! The write-through `Proxy` that `substr-rw` / `subbuf-rw` hand back outside
//! an assignment, so a bound `my $r := substr-rw($s, 1, 1); $r = "Y"` (or the
//! `subbuf-rw` counterpart on a `Buf`) splices into its source.
use super::*;
use crate::ast::{AssignOp, Expr, ParamDef, Stmt};
use crate::symbol::Symbol;

/// Name of the hidden lexical both Proxy closures share for the window length.
const LEN_VAR: &str = "__mutsu_sub_rw_len";
/// Name of the STORE closure's value parameter.
const STORE_VALUE_VAR: &str = "__mutsu_sub_rw_store_value";
/// Names of the hidden lexicals holding the window start and the target value.
const START_VAR: &str = "__mutsu_sub_rw_start";
const TARGET_VAR: &str = "__mutsu_sub_rw_target";

fn plain_param(name: &str) -> ParamDef {
    ParamDef { spans: Default::default(), trait_meta: Default::default(),
        implicit_invocant: Default::default(),
        does_target: Default::default(),
        target: Default::default(),
        type_capture: None,
        name: name.to_string(),
        default: None,
        multi_invocant: false,
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
    }
}

impl Interpreter {
    /// Create a Proxy for `substr-rw` binding. FETCH returns the current
    /// window of the variable named `var_name`; STORE splices into it BY NAME.
    ///
    /// Like rakudo's, the window's length follows each STORE: after
    /// `$r = "ZZ"` the Proxy spans the two new chars, so a later `$r = "W"`
    /// replaces both (#9216). The length lives in a `ContainerRef` cell both
    /// closures capture, so STORE's update is what FETCH and the next STORE
    /// read.
    // Cost: O(n), n = chars of the variable's string (the window is resolved
    // against it once).
    pub(crate) fn make_substr_rw_proxy(
        &mut self,
        var_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = self
            .env
            .get(var_name)
            .cloned()
            .unwrap_or(Value::str(String::new()));
        let str_len = crate::builtins::grapheme_index::with_str_index(&target, |_, idx| idx.len());
        let (start, end) = self.resolve_substr_rw_range(args, str_len)?;
        let target_expr = Expr::Var(var_name.to_string());
        Ok(self.make_sub_rw_proxy("substr", "substr-rw", target_expr, start, end - start, true))
    }

    /// [`Self::make_substr_rw_proxy`] for a source variable the VM handed over
    /// as its shared cell: the Proxy aliases the container, so both closures
    /// are shared compiled bodies and neither names the variable.
    // Cost: O(n), n = chars of the variable's string (the window is resolved
    // against it once).
    pub(crate) fn make_substr_rw_proxy_cell(
        &mut self,
        cell: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = cell.deref_container();
        let str_len = crate::builtins::grapheme_index::with_str_index(&target, |_, idx| idx.len());
        let (start, end) = self.resolve_substr_rw_range(args, str_len)?;
        Ok(self.make_sub_rw_proxy("substr", "substr-rw", Expr::Literal(cell), start, end - start, true))
    }

    /// Create a Proxy for `subbuf-rw` binding (`my $r := $b.subbuf-rw(1, 2)`,
    /// `my $r := subbuf-rw($b, 1, 2)`). The Proxy closes over the buffer
    /// object itself, as rakudo's does over `self`: `subbuf-rw`'s STORE
    /// splices through the buffer's shared storage node, so every alias of the
    /// buffer sees it. Unlike `substr-rw`, rakudo's `subbuf-rw` Proxy keeps its
    /// original window after a STORE, and so does this one.
    // Cost: O(1) beyond resolving the window against the buffer's length.
    pub(crate) fn make_subbuf_rw_proxy(
        &mut self,
        buf: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let elems = match buf.view() {
            ValueView::Instance { attributes, .. } => {
                crate::value::value_buf::buf_elems_or_empty(&attributes).len()
            }
            _ => 0,
        };
        // A `Callable` end is an index into the buffer (`subbuf`'s `&End`),
        // not `substr`'s length.
        let resolved = self.resolve_subbuf_callable_args(&buf, args);
        let args = resolved.as_deref().unwrap_or(args);
        let (start, end) = self.resolve_substr_rw_range(args, elems)?;
        Ok(self.make_sub_rw_proxy(
            "subbuf",
            "subbuf-rw",
            Expr::Literal(buf),
            start,
            end - start,
            false,
        ))
    }

    /// The shared FETCH/STORE pair: FETCH is `TARGET.<read>(START, LEN)`,
    /// STORE is the `<rw>(TARGET, START, LEN) = v` lvalue assignment. With
    /// `track_len`, STORE also moves the window's length to the stored
    /// value's `.chars`.
    ///
    /// The bodies name the target, the window start and the length cell as
    /// captured lexicals, never as literals, so one body per `(read_method,
    /// rw_routine, track_len)` serves every Proxy and is compiled once
    /// (`synthesized_closure_code`): neither closure runs through the
    /// tree-walk carrier. Only the window's length cell is captured besides
    /// the target; a source variable named by `by_name` is still read and
    /// written BY NAME from the caller's live scope (a snapshot of the whole
    /// scope must not ride along: a closure's own capture wins over its
    /// caller's binding (ADR-0055), and a stale `$s` would hide the source's
    /// later writes), which only the carrier provides.
    fn make_sub_rw_proxy(
        &mut self,
        read_method: &str,
        rw_routine: &str,
        target: Expr,
        start: usize,
        len: usize,
        track_len: bool,
    ) -> Value {
        let len_cell = Value::int(len as i64).into_container_ref();
        let by_name = matches!(target, Expr::Var(_));
        let mut env = crate::env::Env::new();
        env.insert(LEN_VAR.to_string(), len_cell);
        // A literal target (the buffer `subbuf-rw` closes over) rides in the
        // captured env; a variable target keeps its by-name read.
        let target_expr = match target {
            Expr::Literal(value) => {
                env.insert(TARGET_VAR.to_string(), value);
                Expr::Var(TARGET_VAR.to_string())
            }
            other => other,
        };
        env.insert(START_VAR.to_string(), Value::int(start as i64));
        let window = || {
            vec![
                Expr::Var(START_VAR.to_string()),
                Expr::Var(LEN_VAR.to_string()),
            ]
        };

        let fetch_body = vec![Stmt::Expr(Expr::MethodCall {
            target: Box::new(target_expr.clone()),
            name: Symbol::intern(read_method),
            args: window(),
            modifier: None,
            quoted: false, sugar: false,
        })];
        let fetch_params = vec!["$".to_string()];
        let fetch_defs = vec![plain_param("$")];
        let fetcher = self.sub_rw_closure(
            &format!("sub-rw:fetch:{read_method}:{by_name}"),
            "__sub_rw_fetch",
            fetch_params,
            fetch_defs,
            fetch_body,
            env.clone(),
            by_name,
        );

        let mut store_body = vec![Stmt::Expr(Expr::Call {
            name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
            args: vec![
                Expr::Literal(Value::str(rw_routine.to_string())),
                Expr::ArrayLiteral([vec![target_expr], window()].concat()),
                Expr::Var(STORE_VALUE_VAR.to_string()),
            ], listop: false,
        })];
        if track_len {
            store_body.push(Stmt::Assign {
                name: LEN_VAR.to_string(),
                expr: Expr::MethodCall {
                    target: Box::new(Expr::Var(STORE_VALUE_VAR.to_string())),
                    name: Symbol::intern("chars"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false, sugar: false,
                },
                op: AssignOp::Assign,
                target_is_sigilless: false,
            });
        }
        let storer = self.sub_rw_closure(
            &format!("sub-rw:store:{rw_routine}:{track_len}:{by_name}"),
            "__sub_rw_store",
            vec!["$".to_string(), STORE_VALUE_VAR.to_string()],
            vec![plain_param("$"), plain_param(STORE_VALUE_VAR)],
            store_body,
            env,
            by_name,
        );

        Value::proxy_parts(fetcher, storer, None, false)
    }

    /// One of the Proxy's two closures. A body that reads no variable by name
    /// has no per-Proxy AST, so it carries the shared compiled chunk; a
    /// by-name body keeps the tree-walk carrier (it needs the caller's live
    /// scope).
    // TODO: compile to bytecode -- the by-name `substr-rw` Proxy needs its source
    // variable as a shared cell (`capture_rw_arg_cell`) instead of a name.
    #[allow(clippy::too_many_arguments)]
    fn sub_rw_closure(
        &mut self,
        key: &str,
        name: &str,
        params: Vec<String>,
        param_defs: Vec<ParamDef>,
        body: Vec<Stmt>,
        env: crate::env::Env,
        by_name: bool,
    ) -> Value {
        if by_name {
            return Value::make_sub(
                Symbol::intern(""),
                Symbol::intern(name),
                params,
                param_defs,
                body,
                false,
                env,
            );
        }
        let (code, fns) = crate::runtime::synthesized_closure_code(key, &params, &param_defs, &body);
        Value::make_sub_compiled(Symbol::intern(name), params, param_defs, body, env, code, fns)
    }
}
