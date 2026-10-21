//! The lexical-introspection `nqp::` ops (#11498): `getlex*`, `bindlex*`,
//! `getlexref_*`, `getlexrel*`, `getlexouter`, `getlexcaller`, `bindlexdyn`,
//! `curlexpad` and `lexprimspec`.
//!
//! Each names a lexical by a runtime string. A frame that contains one is
//! reflective (`nqp_op_names_lexical` in `src/opcode.rs`), so its slots are
//! mirrored into `env` and a by-name lookup there finds the binding a compiled
//! read would. A literal `getlexouter` never reaches this table: the compiler
//! resolves it as `OUTERS::` (`compiler::nqp_lexical_forms`).
//!
//! A context (`nqp::ctx()`) is an env snapshot (`runtime::repl_compiler`), so
//! the ops that take one look the name up in that snapshot.

use super::nqp_attr::NqpAttrConv;
use super::*;
use crate::symbol::Symbol;

/// The routine every native lexical reference FETCHes through.
const LEXREF_FETCH: &str = "__mutsu_native_lexref_fetch";
/// The routine every native lexical reference STOREs through.
const LEXREF_STORE: &str = "__mutsu_native_lexref_store";

/// The env key of a lexical spelled `$x` / `@a` / `%h` / `&f`: a scalar is
/// keyed without its sigil, the containers keep it.
// Cost: O(1).
fn lex_key(name: &str) -> &str {
    name.strip_prefix('$').unwrap_or(name)
}

/// The first operand as a string (the lexical's name).
// Cost: O(m), m = chars of the operand.
fn name_operand(args: &[Value], i: usize) -> String {
    args.get(i).map(|v| v.to_string_value()).unwrap_or_default()
}

/// MoarVM's primitive spec of a declared type: 1 int, 2 num, 3 str, 0 object.
// Cost: O(1).
fn prim_spec_of_type(constraint: &str) -> i64 {
    if crate::runtime::native_types::is_native_int_type(constraint) {
        1
    } else if matches!(constraint, "num" | "num32" | "num64") {
        2
    } else if constraint == "str" {
        3
    } else {
        0
    }
}

/// MoarVM's name for the native lexical reference of form `conv`.
// Cost: O(1).
fn lexref_type_name(conv: NqpAttrConv) -> &'static str {
    match conv {
        NqpAttrConv::Num => "NumLexRef",
        NqpAttrConv::Str => "StrLexRef",
        _ => "IntLexRef",
    }
}

/// The form a native lexical reference type name stands for.
// Cost: O(1).
fn lexref_conv(name: &str) -> Option<NqpAttrConv> {
    Some(match name {
        "IntLexRef" => NqpAttrConv::Int,
        "NumLexRef" => NqpAttrConv::Num,
        "StrLexRef" => NqpAttrConv::Str,
        _ => return None,
    })
}

/// The cell and form a native lexical reference addresses.
// Cost: O(1).
fn lexref_parts(proxy: &Value) -> Result<(Value, NqpAttrConv), RuntimeError> {
    if let ValueView::Proxy {
        subclass: Some((name, attrs)),
        ..
    } = proxy.view()
        && let Some(conv) = lexref_conv(name.as_str())
    {
        let attrs = attrs.as_map();
        if let Some(cell) = attrs.get("cell") {
            return Ok((cell.clone(), conv));
        }
    }
    Err(RuntimeError::new(
        "a native lexical reference was invoked without its lexical",
    ))
}

impl Interpreter {
    /// Try a lexical-introspection `nqp::` op; `None` means "not one of these".
    pub(crate) fn call_nqp_op_lexical(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::getlex($name) / getlex_{i,n,s}: read a lexical of the
            // running scope by its sigiled name, through the same by-name
            // chokepoint a compiled read of a captured lexical uses, so the
            // answer is the cell closures share. The `_i`/`_n`/`_s` forms
            // coerce like `getattr_*`.
            // Cost: O(m + s), m = chars of $name, s = scope-chain depth walked.
            "getlex" | "getlex_i" | "getlex_n" | "getlex_s" => {
                let name = name_operand(args, 0);
                match self.nqp_lex_lookup(&name) {
                    Some(v) => Ok(NqpAttrConv::of_op(op).read(Some(v))),
                    None => Err(RuntimeError::new(format!("Lexical '{name}' not found"))),
                }
            }
            // nqp::getlexouter($name): the lexical as the scopes enclosing the
            // running one see it. A literal name is resolved by the compiler
            // (`OUTERS::`); this arm serves a name only known at run time, and
            // a literal no enclosing scope of the compilation declares (a
            // setting name, or one an `EVAL`'s outer context holds).
            // The enclosing scopes are the ones `nqp::ctxouter` walks
            // (`Env::enclosing_scopes`), so a binding of the running scope
            // that shadows the outer one is skipped; a name none of them
            // holds falls back to the by-name lookup.
            // Cost: O(m + s), m = chars of $name, s = scope-chain depth walked.
            "getlexouter" => {
                let name = name_operand(args, 0);
                let key = lex_key(&name);
                self.env
                    .enclosing_scopes()
                    .iter()
                    .find_map(|scope| scope.get(key))
                    .or_else(|| self.nqp_lex_lookup(&name))
                    .map(Value::into_deref)
                    .ok_or_else(|| {
                        RuntimeError::new(format!("No lexical found with name '{name}'"))
                    })
            }
            // nqp::getlexcaller($name): the lexical as the nearest calling
            // frame that has it sees it; null (Nil) when none does.
            // Cost: O(m + c), m = chars of $name, c = caller frames walked.
            "getlexcaller" => {
                let name = name_operand(args, 0);
                let key = lex_key(&name);
                Ok(self
                    .caller_env_stack
                    .iter()
                    .rev()
                    .find_map(|env| env.get(key))
                    .cloned()
                    .map_or(Value::NIL, Value::into_deref))
            }
            // nqp::bindlexdyn($name, $value): bind the dynamic variable as the
            // caller of the running frame resolves it (the running frame's own
            // declaration is skipped). A dynamic no caller has is an error.
            // Cost: O(m + c), m = chars of $name, c = caller frames walked.
            "bindlexdyn" => {
                let name = name_operand(args, 0);
                let key = lex_key(&name);
                if !self.caller_env_stack.iter().any(|env| env.contains_key(key)) {
                    return Some(Err(RuntimeError::new(format!(
                        "Dynamic variable '{name}' not found"
                    ))));
                }
                let value = args.get(1).cloned().unwrap_or(Value::NIL);
                if self.env.contains_key_own_tier(key) {
                    // The running frame declares its own: bind the caller's
                    // binding only, which the caller drains on return.
                    if let Some(caller) = self.caller_env_stack.last_mut() {
                        caller.insert(key.to_string(), value.clone());
                    }
                    self.record_caller_var_writeback(key);
                } else {
                    // The running frame sees the caller's binding: a by-name
                    // write, propagated back as `::('$*x') = v`'s is.
                    self.env_mut().insert(key.to_string(), value.clone());
                    self.note_caller_env_write(key);
                    // Carried back to the declaring frame across every
                    // return (ADR-12529 §3, slice 40).
                    self.record_runtime_name_write(key);
                }
                Ok(value)
            }
            // nqp::getlexref_{i,n,s}($name): a native reference to the
            // lexical: reads and writes go to the cell the lexical's closures
            // share (the one an atomic op on it uses), coerced like
            // `bindlex_*`. MoarVM's `IntLexRef` / `NumLexRef` / `StrLexRef`.
            // Cost: O(m + l), m = chars of $name, l = the running frame's locals.
            "getlexref_i" | "getlexref_n" | "getlexref_s" => {
                let name = name_operand(args, 0);
                self.nqp_getlexref(&name, NqpAttrConv::of_op(op))
            }
            // nqp::getlexrel($ctx, $name) / getlexreldyn / getlexrelcaller: a
            // lexical looked up in a captured context; the last also walks the
            // context's callers. A miss is an error, as in Rakudo.
            // Cost: O(m + c), m = chars of $name, c = caller contexts walked.
            "getlexrel" | "getlexreldyn" | "getlexrelcaller" => {
                let name = name_operand(args, 1);
                match self.nqp_ctx_lookup(args.first(), &name, op == "getlexrelcaller") {
                    Some(v) => Ok(v),
                    None => Err(RuntimeError::new(format!(
                        "No lexical found with name '{name}'"
                    ))),
                }
            }
            // nqp::lexprimspec($ctx, $name): how the context's lexical is
            // stored: 1 int, 2 num, 3 str (a native declaration), 0 object.
            // Cost: O(m), m = chars of $name.
            "lexprimspec" => {
                let name = name_operand(args, 1);
                let key = lex_key(&name);
                let Some(env) = args.first().and_then(|ctx| self.nqp_ctx_env(ctx)) else {
                    return Some(Err(RuntimeError::new("lexprimspec needs a context")));
                };
                if !env.contains_key(key) {
                    return Some(Err(RuntimeError::new(format!(
                        "Frame has no lexical with name '{name}'"
                    ))));
                }
                let meta = Self::type_meta_key_for_sym(Symbol::intern(key));
                let spec = match env.get_sym(meta).map(Value::view) {
                    Some(ValueView::Str(tc)) => prim_spec_of_type(tc.as_str()),
                    _ => 0,
                };
                Ok(Value::int(spec))
            }
            // nqp::curlexpad(): the lexpad of the current frame, the same hash
            // `nqp::ctxlexpad(nqp::ctx())` answers.
            // Cost: O(v), v = variables visible in the current frame.
            "curlexpad" => {
                let ctx = self.nqp_ctx();
                Ok(self.nqp_ctxlexpad(&[ctx]))
            }
            // nqp::bindlex($name, $value) / bindlex_{i,n,s}: write the lexical
            // in place; a captured lexical's shared cell sees the new value.
            // Cost: O(m + s), m = chars of $name, s = scope-chain depth walked.
            "bindlex" | "bindlex_i" | "bindlex_n" | "bindlex_s" => {
                let name = name_operand(args, 0);
                let key = lex_key(&name);
                if self.get_env_with_main_alias(key).is_none() {
                    return Some(Err(RuntimeError::new(format!(
                        "Lexical '{name}' not found"
                    ))));
                }
                let raw = args.get(1).cloned().unwrap_or(Value::NIL);
                let val = NqpAttrConv::of_op(op).bind(raw);
                self.set_env_with_main_alias(key, val.clone());
                Ok(val)
            }
            _ => return None,
        })
    }

    /// The lexical `name` as a by-name lookup from the running scope sees it;
    /// a routine (`&say`) the setting provides resolves like `&say` does.
    // Cost: O(m + s), m = chars of the name, s = scope-chain depth walked.
    fn nqp_lex_lookup(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.get_env_with_main_alias(lex_key(name)) {
            return Some(v);
        }
        let routine = self.resolve_code_var(name.strip_prefix('&')?);
        (!routine.is_nil()).then_some(routine)
    }

    /// `nqp::getlexref_*`: a `Proxy` subclass named after MoarVM's reference
    /// type, holding the lexical's shared cell.
    // Cost: O(m + l), m = chars of the name, l = the running frame's locals.
    fn nqp_getlexref(&mut self, name: &str, conv: NqpAttrConv) -> Result<Value, RuntimeError> {
        if self.get_env_with_main_alias(lex_key(name)).is_none() {
            return Err(RuntimeError::new(format!(
                "No lexical found with name '{name}'"
            )));
        }
        let Some(cell) = self.atomic_scalar_cell(name, None) else {
            return Err(RuntimeError::new(format!(
                "Lexical with name '{name}' has no container to refer to"
            )));
        };
        let mut attrs = crate::value::AttrMap::new();
        attrs.insert("cell".to_string(), Value::container_ref(cell));
        let routine =
            |name: &str| Value::routine_parts(Symbol::intern(""), Symbol::intern(name), false);
        Ok(Value::proxy_parts(
            routine(LEXREF_FETCH),
            routine(LEXREF_STORE),
            Some((
                Symbol::intern(lexref_type_name(conv)),
                crate::value::proxy_subclass_attrs(Symbol::intern(lexref_type_name(conv)), attrs),
            )),
            false,
        ))
    }

    /// Dispatch one of the two routines a native lexical reference calls;
    /// `None` for any other name.
    // Cost: O(1) per access (one cell read or write).
    pub(crate) fn try_native_lex_ref_routine(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let fetch = match name {
            LEXREF_FETCH => true,
            LEXREF_STORE => false,
            _ => return None,
        };
        Some(
            lexref_parts(args.first().unwrap_or(&Value::NIL)).map(|(cell, conv)| {
                if fetch {
                    return conv.read(Some(cell.into_deref()));
                }
                let val = conv.bind(args.get(1).cloned().unwrap_or(Value::NIL).deref_container());
                if let ValueView::ContainerRef(cell) = cell.view() {
                    Value::store_through_cell(&cell, &val);
                }
                val
            }),
        )
    }
}
