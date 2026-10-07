//! `Metamodel::*HOW` methods, part 4 (ADR-11276 slice 3G): one Interpreter method per
//! metamethod. The rows that reach them are `method_table/ctors_mop/class_how.rs`.

use super::methods_classhow_dispatch::{unwrap_method_instance_callable};
use super::*;

impl Interpreter {
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_add_method(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let class_name = match args[0].view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                _ => {
                    return Err(RuntimeError::new("add_method target must be a type object"));
                }
            };
            // A **qualified spelling of an already-registered class** adds to
            // that class, not to a fresh stub under the long name.
            // `NativeHelpers::Pointer` adds pointer arithmetic with
            // `NativeCall::Types::Pointer.^add_method('add', …)`, while the
            // prelude registers `Pointer` under its short name and tags every
            // handle with it — so the stub was created, populated, and never
            // consulted, leaving `.add` "no such method" and `.succ`/`.pred`
            // falling through to the numeric successor.
            let class_name = match Some(
                crate::qualified::last_segment(crate::qualified::known_symbol(&class_name))
                    .as_str(),
            ) {
                Some(short)
                    if short != class_name
                        && !self.registry().classes.contains_key(&class_name)
                        && self.registry().classes.contains_key(short) =>
                {
                    short.to_string()
                }
                _ => class_name,
            };
            let method_name = args[1].to_string_value();
            let method_value = unwrap_method_instance_callable(&args[2]);
            if let ValueView::Routine {
                package,
                name,
                is_regex: true,
                ..
            } = method_value.view()
            {
                let source_key = crate::runtime::dispatch_key::qualified_intern(
                    &package.resolve(),
                    &name.resolve(),
                );
                let Some(source_defs) = self.registry().token_defs.get(&source_key).cloned()
                else {
                    return Ok(Value::NIL);
                };
                let target_key =
                    crate::runtime::dispatch_key::qualified_intern(&class_name, &method_name);
                let target_defs = source_defs
                    .iter()
                    .map(|source| {
                        let mut def = (**source).clone();
                        def.package = Symbol::intern(&class_name);
                        def.name = Symbol::intern(&method_name);
                        def.decl_order = crate::runtime::resolution::next_decl_order();
                        std::sync::Arc::new(def)
                    })
                    .collect();
                std::sync::Arc::make_mut(&mut self.registry_mut().token_defs)
                    .insert(target_key, target_defs);
                crate::runtime::regex_parse::TOKEN_DEFS_GEN
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                return Ok(Value::NIL);
            }
            // A regex value (`EVAL 'regex { a | b }'`, `token { ... }`)
            // becomes the type's grammar rule `method_name`, exactly as a
            // `regex x { ... }` declared in its body would: `.parse` and a
            // `<x>` subrule resolve it through `token_defs`. A declarator
            // term's signature becomes the rule's.
            // Cost: O(p), p = the regex's parameters.
            if matches!(
                method_value.view(),
                ValueView::Regex(..) | ValueView::RegexWithAdverbs(..)
            ) {
                let param_defs: Vec<crate::ast::ParamDef> = method_value
                    .regex_signature()
                    .map(|sig| (*sig).clone())
                    .unwrap_or_default();
                let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
                let body = vec![Stmt::Expr(Expr::Literal(method_value.clone()))];
                self.register_token_decl_in(
                    &class_name,
                    &method_name,
                    &params,
                    &param_defs,
                    &body,
                    false,
                    None,
                );
                return Ok(Value::NIL);
            }
            // A builtin/operator code value is a name-only Routine rather
            // than a Sub with an AST body. Materialize it as a plain
            // forwarding block once, so the existing add_method path can
            // bind the invocant and compile the method normally. This is
            // the shape used by Version::Raku's `&[cmp]`/`&[eqv]` aliases.
            let method_value = if let ValueView::Routine {
                package,
                name,
                is_regex: false,
                ..
            } = method_value.view()
            {
                // Only the arity is taken: the forwarder dispatches by name
                // across the whole family, so keeping one user candidate's
                // typed `param_defs` (`multi infix:<==>(Foo:D, Foo:D)`
                // declared elsewhere) would reject the builtin's operands.
                let (declared, _) = self.callable_signature(&method_value);
                let params: Vec<String> =
                    (0..declared.len()).map(|i| format!("arg{i}")).collect();
                let param_defs = Vec::new();
                let call_name = if crate::qualified::is_global_package(package) {
                    name
                } else {
                    crate::qualified::qualified(package, name)
                };
                // Call through the code value (`&infix:<==>(..)`), which
                // dispatches the whole family -- builtin operands included --
                // exactly like calling the `&[==]` value itself.
                let body = vec![Stmt::Expr(Expr::CallOn {
                    target: Box::new(Expr::CodeVar(call_name.resolve())),
                    args: params.iter().cloned().map(Expr::Var).collect(),
                })];
                Value::make_sub(package, name, params, param_defs, body, false, Env::new())
            } else {
                method_value
            };
            let ValueView::Sub(sub_data) = method_value.view() else {
                return Ok(Value::NIL);
            };
            // `^find_method` on a *multi* method family returns its first
            // candidate as a carrier Sub tagged with `__mutsu_lookup_class`
            // / `__mutsu_lookup_method` and no candidate index. Registering
            // just that carrier would freeze the alias to one signature
            // (Text::CSV's BEGIN-time `alias` helper maps `column-names`
            // onto the four-candidate `column_names` multi). Clone the
            // whole candidate family for the new name instead.
            let multi_family: Option<Vec<MethodDef>> = (|| {
                if sub_data.env.get("__mutsu_lookup_candidate_idx").is_some() {
                    return None;
                }
                let ValueView::Str(src_class) =
                    sub_data.env.get("__mutsu_lookup_class").map(Value::view)?
                else {
                    return None;
                };
                let ValueView::Str(src_method) =
                    sub_data.env.get("__mutsu_lookup_method").map(Value::view)?
                else {
                    return None;
                };
                // ADR-0019 F4a: `src_class` can name a role directly
                // (`R.^find_method('m')` with `R` never `.new`-punned or
                // `does`-composed anywhere), which has no row in the
                // canonical method table -- the role fallback is required
                // here, not optional, confirmed against real Rakudo (a
                // role-owned multi aliased this way keeps every
                // candidate, not just the carrier's own signature).
                self.registry()
                    .get_method_overloads_with_role_fallback(
                        src_class.as_ref(),
                        src_method.as_ref(),
                    )
                    .filter(|defs| defs.iter().any(|d| d.is_multi))
            })();
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
                    defs.push(super::methods_format::positional_param(&format!(
                        "${}",
                        name.trim_start_matches('$')
                    )));
                }
                if let Some(first) = defs.first_mut()
                    && !first.named
                    && !first.slurpy
                    && !first.double_slurpy
                {
                    first.is_invocant = true;
                }
                defs
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
            let named_invocant: Option<String> = (!takes_positional_invocant)
                .then(|| {
                    sub_data
                        .param_defs
                        .iter()
                        .find(|pd| pd.is_invocant)
                        .map(|pd| pd.name.trim_start_matches(['$', '\\']).to_string())
                        .filter(|n| !n.is_empty() && n != "self")
                })
                .flatten();
            let (method_body, method_compiled) = match named_invocant {
                Some(inv_name) => {
                    let mut body = vec![
                        crate::ast::Stmt::VarDecl {
                            name: inv_name.clone(),
                            expr: crate::ast::Expr::BareWord("self".to_string()),
                            type_constraint: None,
                            is_state: false,
                            is_our: false,
                            is_dynamic: false,
                            is_export: false,
                            export_tags: Vec::new(),
                            custom_traits: Vec::new(),
                            where_constraint: None,
                        },
                        crate::ast::Stmt::MarkSigillessReadonly(inv_name),
                    ];
                    body.extend(sub_data.body.iter().cloned());
                    (std::sync::Arc::new(body), None)
                }
                None => (sub_data.body.clone(), sub_data.compiled_code.clone()),
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
            let method_compiled = match (&method_compiled, sub_data.compiled_routine.as_ref()) {
                (None, Some(routine)) if method_body.is_empty() => {
                    Some(routine.code.clone())
                }
                _ => method_compiled,
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
            let filtered_params: Vec<String> = if takes_positional_invocant {
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
                env.insert("__mutsu_declared_method_capture".to_string(), Value::int(1));
                Some(env)
            };
            let def = MethodDef {
                syms: Default::default(),
                lexical_package: sub_data.package,
                params: filtered_params,
                param_defs: filtered_param_defs,
                body: method_body,
                is_rw: sub_data.is_rw,
                is_raw: sub_data.is_raw,
                is_private: false,
                is_multi: false,
                is_my: false,
                role_origin: None,
                original_role: None,
                return_type: None,
                compiled_code: method_compiled,
                compiled_fns: None,
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
            };
            // A role's methods live in its `RoleDef`, which is what
            // composition (`does`, `but`, `.^mixin`) copies into the
            // consumer. Writing them to a stub class of the role's name
            // made `R.^add_method(...)` invisible to every composer --
            // above all a role built with `Metamodel::ParametricRoleHOW`
            // (Tinky's per-workflow transition role).
            if !self.registry().classes.contains_key(&class_name)
                && self.registry().roles.contains_key(&class_name)
            {
                let defs = multi_family.unwrap_or_else(|| vec![def]);
                return self.add_methods_to_role(&class_name, &method_name, defs);
            }
            // If the class doesn't exist yet (e.g. built-in types like Rat, Int, Str),
            // create a stub ClassDef so methods can be added dynamically.
            if !self.registry().classes.contains_key(&class_name) {
                self.registry_mut().classes.insert(
                    class_name.clone(),
                    ClassDef {
                        parents: vec![],
                        attributes: vec![],
                        attribute_types: HashMap::new(),
                        attribute_smileys: HashMap::new(),
                        attribute_built: HashMap::new(),
                        embedded_attributes: HashSet::new(),
                        alias_attributes: HashSet::new(),
                        native_methods: HashSet::new(),
                        mro: sym_mro(&[&class_name]),
                        wildcard_handles: vec![],
                        class_level_attrs: ValueMap::default(),
                    }.into(),
                );
            }
            let mut defs = multi_family.unwrap_or_else(|| vec![def]);
            // `add_method` adds a *public* method. A private `method !name`
            // is a separate namespace in Raku, but mutsu keys it under the
            // same name, so replacing the name's candidate list would drop
            // it: a monitor (OO::Monitors re-adds every declared method)
            // with both `method stop` and `method !stop` lost `self!stop`
            // (Timer::Stopwatch).
            if let Some(existing) = self
                .registry()
                .user_method_overloads(&class_name, &method_name)
            {
                defs.extend(existing.into_iter().filter(|def| def.is_private));
            }
            self.registry_mut().set_user_methods(
                Symbol::intern(&class_name),
                Symbol::intern(&method_name),
                defs,
            );
            // Class shape changed (an added BUILD/TWEAK/new flips ctor
            // eligibility) — drop cached construction plans.
            self.caches.native_ctor_plan_cache.clear();
            // Return Nil even if the class was not found (e.g. built-in types
            // like Rat that are not in the user-defined class registry).
            // Raku's add_method returns the method name; returning Nil is
            // sufficient for eval-lives-ok tests.
            Ok(Value::NIL)
    }
}
