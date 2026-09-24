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

fn plain_param(name: &str) -> ParamDef {
    ParamDef {
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

    /// The shared FETCH/STORE pair: FETCH is `TARGET.<read>(start, len)`,
    /// STORE is the `<rw>(TARGET, start, len) = v` lvalue assignment. With
    /// `track_len`, STORE also moves the window's length to the stored
    /// value's `.chars`.
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
        let mut env = self.env.clone();
        env.insert(LEN_VAR.to_string(), len_cell);
        let window = || {
            vec![
                Expr::Literal(Value::int(start as i64)),
                Expr::Var(LEN_VAR.to_string()),
            ]
        };

        let fetch_body = vec![Stmt::Expr(Expr::MethodCall {
            target: Box::new(target.clone()),
            name: Symbol::intern(read_method),
            args: window(),
            modifier: None,
            quoted: false,
        })];
        let fetcher = Value::make_sub(
            Symbol::intern(""),
            Symbol::intern("__sub_rw_fetch"),
            vec!["$".to_string()],
            vec![plain_param("$")],
            fetch_body,
            false,
            env.clone(),
        );

        let mut store_body = vec![Stmt::Expr(Expr::Call {
            name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
            args: vec![
                Expr::Literal(Value::str(rw_routine.to_string())),
                Expr::ArrayLiteral([vec![target], window()].concat()),
                Expr::Var(STORE_VALUE_VAR.to_string()),
            ],
        })];
        if track_len {
            store_body.push(Stmt::Assign {
                name: LEN_VAR.to_string(),
                expr: Expr::MethodCall {
                    target: Box::new(Expr::Var(STORE_VALUE_VAR.to_string())),
                    name: Symbol::intern("chars"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                },
                op: AssignOp::Assign,
            });
        }
        let storer = Value::make_sub(
            Symbol::intern(""),
            Symbol::intern("__sub_rw_store"),
            vec!["$".to_string(), STORE_VALUE_VAR.to_string()],
            vec![plain_param("$"), plain_param(STORE_VALUE_VAR)],
            store_body,
            false,
            env,
        );

        Value::proxy_parts(fetcher, storer, None, false)
    }
}
