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
    /// Create a Proxy for `substr-rw` binding over the source variable's shared
    /// cell, which the VM hands over: FETCH returns the current window of the
    /// cell's string; STORE splices into it. Like rakudo's, the window's length
    /// follows each STORE: after `$r = "ZZ"` the Proxy spans the two new chars,
    /// so a later `$r = "W"` replaces both (#9216). The length lives in a
    /// `ContainerRef` cell both closures capture.
    // Cost: O(n), n = chars of the variable's string (the window is resolved
    // against it once).
    pub(crate) fn make_substr_rw_proxy(
        &mut self,
        cell: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = cell.deref_container();
        let str_len = crate::builtins::grapheme_index::with_str_index(&target, |_, idx| idx.len());
        let (start, end) = self.resolve_substr_rw_range(args, str_len)?;
        Ok(self.make_sub_rw_proxy("substr", "substr-rw", cell, start, end - start, true))
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
            buf,
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
    /// tree-walk carrier. The target is the source variable's shared cell (or
    /// the buffer itself), captured, so no closure names a variable of the
    /// caller's live scope.
    fn make_sub_rw_proxy(
        &mut self,
        read_method: &str,
        rw_routine: &str,
        target: Value,
        start: usize,
        len: usize,
        track_len: bool,
    ) -> Value {
        let mut env = crate::env::Env::new();
        env.insert(LEN_VAR.to_string(), Value::int(len as i64).into_container_ref());
        env.insert(START_VAR.to_string(), Value::int(start as i64));
        env.insert(TARGET_VAR.to_string(), target);
        let target_expr = || Expr::Var(TARGET_VAR.to_string());
        let window = || {
            vec![
                Expr::Var(START_VAR.to_string()),
                Expr::Var(LEN_VAR.to_string()),
            ]
        };

        let fetch_body = vec![Stmt::Expr(Expr::MethodCall {
            target: Box::new(target_expr()),
            name: Symbol::intern(read_method),
            args: window(),
            modifier: None,
            quoted: false, sugar: false,
        })];
        let fetcher = Self::sub_rw_closure(
            &format!("sub-rw:fetch:{read_method}"),
            "__sub_rw_fetch",
            (vec!["$".to_string()], vec![plain_param("$")]),
            fetch_body,
            env.clone(),
        );

        let mut store_body = vec![Stmt::Expr(Expr::Call {
            name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
            args: vec![
                Expr::Literal(Value::str(rw_routine.to_string())),
                Expr::ArrayLiteral([vec![target_expr()], window()].concat()),
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
        let storer = Self::sub_rw_closure(
            &format!("sub-rw:store:{rw_routine}:{track_len}"),
            "__sub_rw_store",
            (
                vec!["$".to_string(), STORE_VALUE_VAR.to_string()],
                vec![plain_param("$"), plain_param(STORE_VALUE_VAR)],
            ),
            store_body,
            env,
        );

        Value::proxy_parts(fetcher, storer, None, false)
    }

    /// One of the Proxy's two closures: the shared compiled chunk for `key`
    /// over the Proxy's own captured `env`.
    // Cost: O(1) after the first request for `key`.
    fn sub_rw_closure(
        key: &str,
        name: &str,
        (params, param_defs): (Vec<String>, Vec<ParamDef>),
        body: Vec<Stmt>,
        env: crate::env::Env,
    ) -> Value {
        let chunk = crate::runtime::synthesized_closure_code(key, &params, &param_defs, &body);
        Value::make_sub_compiled(Symbol::intern(name), params, param_defs, body, env, chunk)
    }
}
