//! Building the `MethodDef` that `^add_method` / `^add_private_method` /
//! `^add_multi_method` install from the code value they are handed.

use super::*;
use crate::value::SubData;

impl Interpreter {
    /// The method definition for code `sub_data` installed by a
    /// `Metamodel::*HOW` method-adding call: the invocant parameter (an
    /// implicit one for a `method`, the first positional for a block or
    /// `sub`), the declared routine's bytecode, and the closure's captures.
    /// `multi` marks a candidate of a multi family (`^add_multi_method`).
    // Cost: O(p + b), p = the code's parameters, b = its body statements
    // (cloned only when a binding is prepended).
    pub(super) fn method_def_from_code(
        &mut self,
        sub_data: &SubData,
        private: bool,
        multi: bool,
    ) -> MethodDef {
        // A plain block -- or a `sub`, anonymous or DECLARED
        // (`&named-sub`) -- passed to `^add_method` receives the
        // invocant as its first positional argument. Unlike a `method`
        // literal, that parameter is visible in the code's signature
        // (`A.^add_method('m', -> $x {...})` and `sub ($x) {...}` have
        // `($x)`, not an implicit `self` plus `$x`). PDF::COS::Tie
        // installs every entry accessor as `sub (\obj) is rw {...}`
        // (#9479). Mark that parameter as the method's invocant, so the
        // method binder binds it to the receiver by name at dispatch
        // (`call_compiled_method`'s invocant arm), exactly like
        // `method ($inv: ...)`. The code itself is not rewritten, so
        // this works the same for a declared routine, whose bytecode
        // lives in `compiled_routine` with no AST, and for a parameter
        // literally named `$self` (#9549).
        // A method -- a `method`/`submethod` literal, a `^find_method`
        // carrier, or any code declaring an invocant parameter -- keeps
        // the implicit-invocant handling.
        let is_method_code = matches!(
            sub_data
                .env
                .get_sym(crate::symbol::well_known::callable_type())
                .map(Value::view),
            Some(ValueView::Str(kind)) if matches!(kind.as_str(), "Method" | "Submethod")
        ) || sub_data.env.get("__mutsu_lookup_class").is_some()
            || sub_data.param_defs.iter().any(|pd| pd.is_invocant);
        let takes_positional_invocant = !is_method_code;
        // A multi candidate keeps a method's own invocant parameter: its type
        // and smiley (`(::T Mu:U:)` vs `(Mu:D:)`) take part in choosing the
        // candidate, where a single method has nothing to choose between.
        let keep_invocant = multi && is_method_code;
        let filtered_param_defs: Vec<ParamDef> = if takes_positional_invocant {
            let mut defs: Vec<ParamDef> = sub_data.param_defs.to_vec();
            // Code with a names-only signature (a one-parameter pointy
            // block `-> $r {...}`, a builtin routine like `&[cmp]`)
            // carries its parameter NAMES (`params`) but no `ParamDef`s.
            // Give the first one a def so there is something to mark as
            // the invocant; the binder pairs `params[i]` with
            // `param_defs[i]`, so the rest keep binding by name alone.
            if defs.is_empty()
                && let Some(name) = sub_data.params.first()
            {
                // A parameter literally named `$self` stays `self`, the
                // spelling the binder recognises as the `$self` lexical.
                let bare = name.trim_start_matches('$');
                let spelled = if bare == "self" { bare.to_string() } else { format!("${bare}") };
                defs.push(super::methods_format::positional_param(&spelled));
            }
            if let Some(first) = defs.first_mut()
                && !first.named
                && !first.slurpy
                && !first.double_slurpy
            {
                first.is_invocant = true;
            }
            defs
        } else if keep_invocant {
            sub_data.param_defs.to_vec()
        } else {
            sub_data
                .param_defs
                .iter()
                .filter(|pd| !pd.is_invocant)
                .cloned()
                .collect()
        };
        // A NAMED invocant other than `self` (`anon method (Mu \SELF:
        // |) {...}` — OO::Monitors' POPULATE hook) is dropped from the
        // params like any invocant, but the body refers to it by name,
        // so prepend a `SELF := self` binding and let the dispatch
        // recompile the adjusted body on demand.
        let named_invocant: Option<String> = (!takes_positional_invocant && !keep_invocant)
            .then(|| {
                sub_data
                    .param_defs
                    .iter()
                    .find(|pd| pd.is_invocant)
                    .map(|pd| pd.name.trim_start_matches(['$', '\\']).to_string())
                    .filter(|n| !n.is_empty() && n != "self")
            })
            .flatten();
        // A `sub` (not a method) refers to the `self` of the scope that
        // created it, never to the receiver it is later installed on:
        // PDF::COS::Tie's `my &accessor = sub (\obj) is rw {
        // obj.rw-accessor(self, :$key) }` passes the Attribute the
        // accessor was made by. The method binder sets `self` to the
        // receiver, so rebind the captured one at the head of the body.
        // TODO: a declared routine (`&named-sub`) has no AST body to
        // prepend to; its `self` is still the receiver.
        let lexical_self = takes_positional_invocant
            .then(|| sub_data.fixtures.as_ref().and_then(|f| f.invocant.clone()))
            .flatten()
            .filter(|_| !sub_data.body.is_empty());
        let (method_body, method_compiled) = match (named_invocant, lexical_self) {
            (None, Some(outer_self)) => {
                let mut body = vec![crate::ast::Stmt::VarDecl { marks: Default::default(),
                    anonymous: false,
                    name: "self".to_string(),
                    expr: crate::ast::Expr::Literal(outer_self),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    has_initializer: false,
                    constant: None,
                    bind: None,
                    where_constraint: None,
                }];
                body.extend(sub_data.body.iter().cloned());
                (std::sync::Arc::new(body), None)
            }
            (Some(inv_name), _) => {
                let mut body = vec![
                    crate::ast::Stmt::VarDecl { marks: crate::ast::var_marks::VarMarks {
                            term: crate::ast::var_marks::TermMark::Readonly,
                            ..Default::default()
                        },
                        anonymous: false,
                        name: inv_name.clone(),
                        expr: crate::ast::Expr::BareWord("self".to_string()),
                        type_constraint: None,
                        is_state: false,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                        custom_traits: Vec::new(),
                        has_initializer: false,
                        constant: None,
                        bind: None,
                        where_constraint: None,
                    },
                ];
                body.extend(sub_data.body.iter().cloned());
                (std::sync::Arc::new(body), None)
            }
            (None, None) => (sub_data.body.clone(), sub_data.compiled_code.clone()),
        };
        // A Sub built from a DECLARED routine (`my method m() {...}`,
        // `my sub a() {...}`, `&foo` -- anything read back through
        // `GetCodeVar`) carries its bytecode in `compiled_routine`, not
        // in `compiled_code`: ADR-0019 C6c stopped the declaration plan
        // shipping an executable AST, and `SubData` keeps the two apart
        // because they are invoked under different calling conventions.
        // Without this, `add_method` registered a `MethodDef` with an
        // empty body and no code, so the method was findable by
        // `.^can`/`.^lookup` and answered `Nil` when called -- silently.
        // Only the anonymous shapes (`method () {...}`, a pointy block)
        // ever worked. See `t/add-method-named-routine.t`.
        // The subs nested in that body travel with the code.
        let (method_compiled, method_compiled_fns) =
            match (&method_compiled, sub_data.compiled_routine.as_ref()) {
                (None, Some(routine)) if method_body.is_empty() => {
                    (Some(routine.code.clone()), routine.compiled_fns.clone())
                }
                (Some(_), _) => (method_compiled, sub_data.compiled_fns.clone()),
                _ => (method_compiled, None),
            };
        // The name list has to lose the invocant too, not just
        // `param_defs`: dispatch binds arguments positionally against
        // `params`, so leaving `self` in it shifted every argument by one
        // and left the last parameter undeclared — `method (Pointer:D:
        // Int $off) { … $off … }` died with "Variable 'off' is not
        // declared" (`NativeHelpers::Pointer`'s `add`).
        let invocant_names: HashSet<&str> = sub_data
            .param_defs
            .iter()
            .filter(|pd| pd.is_invocant)
            .map(|pd| pd.name.as_str())
            .collect();
        let filtered_params: Vec<String> = if takes_positional_invocant || keep_invocant {
            sub_data.params.to_vec()
        } else {
            sub_data
                .params
                .iter()
                .filter(|p| {
                    !invocant_names.contains(p.trim_start_matches(['$', '@', '%', '&']))
                })
                .cloned()
                .collect()
        };
        let captured_env = if sub_data.env.is_empty() {
            None
        } else {
            // A closure handed to ^add_method carries lexical values
            // from the code that created it.  Mark that capture as
            // authoritative so a nested method call cannot let a
            // same-named lexical from its caller shadow it.
            let mut env = sub_data.env.clone();
            env.insert("__mutsu_declared_method_capture".to_string(), Value::int(2));
            crate::term_names::bind_lexical_types_by_bare_name(&mut env);
            Some(env)
        };
        MethodDef {
            value_prototype: None,
            syms: Default::default(),
            lexical_package: sub_data.package,
            params: (filtered_params).into(),
            param_defs: (filtered_param_defs).into(),
            body: method_body,
            is_rw: sub_data.is_rw,
            is_raw: sub_data.is_raw,
            is_private: private,
            is_multi: multi,
            is_my: false,
            role_origin: None,
            original_role: None,
            return_type: None,
            compiled_code: method_compiled,
            compiled_fns: method_compiled_fns,
            delegation: None,
            is_default: false,
            deprecated_message: None,
            is_submethod: false,
            is_hidden_from_backtrace: false,
            // Preserve the closure literal's captured scope so a method
            // like `method { attr.get_value(self) }` (Attribute::Predicate's
            // `is predicate`) can still resolve `attr` after its creating
            // sub returns. Only carried when the env actually holds captures.
            captured_env,
            source_file: sub_data.source_file.clone(),
            role_param_bindings: None,
            nested_capture_index: None,
            captured_readonly: None,
            routine_cell: Default::default(),
            augment_epoch: 0,
        }
    }
}
