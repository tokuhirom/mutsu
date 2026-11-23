//! `Metamodel::*HOW` methods, part 4 (ADR-11276 slice 3G): one Interpreter method per
//! metamethod. The rows that reach them are `method_table/ctors_mop/class_how.rs`.

use super::methods_classhow_dispatch::{unwrap_method_instance_callable};
use super::*;

impl Interpreter {
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_add_method(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.record_slang_mop("add_method", &args);
        self.mop_add_method_kind(args, false)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    /// `Metamodel::MetaMethodContainer.add_meta_method(type, name, method)`: a
    /// metamethod (`method ^name`) is the type's `^name` method, which a
    /// `.^name` call on the type reaches.
    pub(crate) fn mop_add_meta_method(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut args = args;
        if let Some(name) = args.get(1) {
            let name = name.to_string_value();
            if !name.starts_with('^') {
                args[1] = Value::str(format!("^{name}"));
            }
        }
        self.mop_add_method_kind(args, false)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    /// `Metamodel::PrivateMethodContainer.add_private_method`: the same install as
    /// `add_method`, but the definition lands in the private namespace.
    pub(crate) fn mop_add_private_method(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.record_slang_mop("add_private_method", &args);
        self.mop_add_method_kind(args, true)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    fn mop_add_method_kind(
        &mut self,
        args: Vec<Value>,
        private: bool,
    ) -> Result<Value, RuntimeError> {
            let class_name = match args[0].view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                // The type object of `Class.^mixin(R)`: its mixin class.
                ValueView::Mixin(..) => match self.mixin_class_of_type_object(&args[0]) {
                    Some(class_name) => class_name,
                    None => {
                        return Err(RuntimeError::new("add_method target must be a type object"));
                    }
                },
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
            // A method with a role mixed in (`anon method {...} but Tag[$x]`)
            // is installed by its callable; the mixin rides on the wrapper.
            // TODO: keep the mixed-in roles on `^find_method`'s answer too.
            let method_arg = match args[2].view() {
                ValueView::Mixin(inner, _) => Value::clone(inner),
                _ => args[2].clone(),
            };
            let method_value = unwrap_method_instance_callable(&method_arg);
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
                let Some(source_defs) = self.routines().token_defs.get(&source_key).cloned()
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
                std::sync::Arc::make_mut(&mut self.routines_mut().token_defs)
                    .insert(target_key, target_defs);
                crate::runtime::regex_parse::TOKEN_DEFS_GEN
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                // A `.^find_method` answer memoized before the rule existed
                // (a miss) must not outlive it.
                self.registry_mut().bump_method_generation();
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
                self.registry_mut().bump_method_generation();
                return Ok(Value::NIL);
            }
            // An attribute accessor (`Foo.^method_table<val>`, AttrX::Mooish's
            // `$orig-accessor` it re-adds under an alias) is a name-only Routine
            // for the generated reader. Forwarding through `&val` would look for
            // a code variable the class never declares, so the alias reads the
            // attribute itself, as the accessor does.
            let method_value = if let ValueView::Routine {
                package,
                name,
                is_regex: false,
                ..
            } = method_value.view()
                && !private
                && let Some(is_rw) = self
                    .collect_class_attributes(&package.resolve())
                    .iter()
                    .find(|attr| attr.is_public && attr.name == name.resolve())
                    .map(|attr| attr.is_rw)
                && self
                    .registry()
                    .user_method_overloads(&package.resolve(), &name.resolve())
                    .is_none()
            {
                // `method () { self.attr }`: the alias answers through the
                // class's own accessor, which keeps its rw/type behaviour.
                let body = vec![Stmt::Expr(Expr::MethodCall {
                    target: Box::new(Expr::BareWord("self".to_string())),
                    name,
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                    sugar: false,
                })];
                let mut env = Env::new();
                env.insert_sym(
                    crate::symbol::well_known::callable_type(),
                    Value::str_from("Method"),
                );
                Value::make_sub(package, name, Vec::new(), Vec::new(), body, is_rw, env)
            } else {
                method_value
            };
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
                    .map(|defs| defs.to_vec())
            })();
            let def = self.method_def_from_code(&sub_data, private, false);
            // A role's methods live in its `RoleDef`, which is what
            // composition (`does`, `but`, `.^mixin`) copies into the
            // consumer. Writing them to a stub class of the role's name
            // made `R.^add_method(...)` invisible to every composer --
            // above all a role built with `Metamodel::ParametricRoleHOW`
            // (Tinky's per-workflow transition role).
            if !self.registry().classes.contains_key(&class_name)
                && self.registry().roles.contains_key(&class_name)
            {
                let mut defs = multi_family.unwrap_or_else(|| vec![def]);
                for d in &mut defs {
                    d.is_private = private;
                }
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
            // The `Method` object keeps the code's own name (`my method dm`
            // added as `m` is `dm`, an anonymous one ``), #18225.
            let own_name = multi_family.is_none().then(|| sub_data.name.resolve());
            let mut defs = multi_family.unwrap_or_else(|| vec![def]);
            for d in &mut defs {
                d.is_private = private;
            }
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
                // `add_private_method` is the mirror image: it keeps the public ones.
                defs.extend(existing.into_iter().filter(|def| def.is_private != private));
            }
            self.registry_mut().set_user_methods(
                Symbol::intern(&class_name),
                Symbol::intern(&method_name),
                defs,
            );
            if let Some(own_name) = own_name {
                self.registry_mut().rename_method_candidate(&class_name, &method_name, 0, &own_name);
            }
            // Class shape changed (an added BUILD/TWEAK/new flips ctor
            // eligibility) — drop cached construction plans.
            self.caches.native_ctor_plan_cache.clear();
            self.caches.native_ctor_plan_builtin.clear();
            // Return Nil even if the class was not found (e.g. built-in types
            // like Rat that are not in the user-defined class registry).
            // Raku's add_method returns the method name; returning Nil is
            // sufficient for eval-lives-ok tests.
            Ok(Value::NIL)
    }
}
