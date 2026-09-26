//! The synthesized-callable idiom the supply machinery uses to hand a native
//! operation to code that only knows how to call a `Callable`: an empty-env
//! `SubData` whose body is a single `MethodCall` on a literal internal
//! instance (`__ScheduledTapPump`, `__SupplyCollector`,
//! `__SupplyQuitForwarder`, `__SupplyDerive`). Every delivery path — tap
//! callbacks, `done`/`quit` handlers, close callbacks — dispatches it
//! uniformly through `call_sub_value`, which is why it is a real callable
//! rather than a marker value that each path would have to special-case.

use crate::runtime::*;
use crate::symbol::Symbol;

/// Build the shim: `-> $v { instance.method_name($v) }` when `has_param`,
/// otherwise `{ instance.method_name() }`.
pub(in crate::runtime) fn native_method_shim(
    instance: Value,
    method_name: &str,
    has_param: bool,
) -> Value {
    let (params, param_defs, call_args) = if has_param {
        (
            vec!["v".to_string()],
            vec![crate::ast::ParamDef {
                type_capture: None,
                name: "v".to_string(),
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
                trait_args: Vec::new(),
            }],
            vec![crate::ast::Expr::Var("v".to_string())],
        )
    } else {
        (Vec::new(), Vec::new(), Vec::new())
    };
    let body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
        target: Box::new(crate::ast::Expr::Literal(instance)),
        name: Symbol::intern(method_name),
        args: call_args,
        modifier: None,
        quoted: false,
    })];
    Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
        package: Symbol::intern("GLOBAL"),
        name: Symbol::intern(""),
        params: std::sync::Arc::new(params),
        param_defs: std::sync::Arc::new(param_defs),
        body: std::sync::Arc::new(body),
        is_rw: false,
        is_raw: false,
        env: Env::new(),
        assumed_positional: Vec::new(),
        assumed_named: ValueMap::default(),
        id: crate::value::next_instance_id(),
        empty_sig: false,
        is_bare_block: true,
        compiled_code: None,
        compiled_fns: None,
        compiled_routine: None,
        is_decl_expr_thunk: false,
        deprecated_message: None,
        source_line: None,
        source_file: None,
        owned_captures: Vec::new(),
        authoritative_captures: Vec::new(),
        upvalues: Vec::new(),
        captured_fatal_mode: false,
        param_name_syms_cache: std::sync::OnceLock::new(),
        source_file_sym_cache: std::sync::OnceLock::new(),
        state_scope_guard: None,
    }))
}
