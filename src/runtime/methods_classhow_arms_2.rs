//! `Metamodel::*HOW` methods, part 2 (ADR-11276 slice 3G): one Interpreter method per
//! metamethod. The rows that reach them are `method_table/ctors_mop/class_how.rs`.

use super::*;

impl Interpreter {
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_refinee(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            let Some(base) = self
                .registry()
                .subsets
                .get(&name)
                .map(|subset| subset.base.clone())
            else {
                let how = self.dispatch_how(&args[0], &[])?;
                let how_name = match how.view() {
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => "Mu".to_string(),
                };
                return Err(crate::runtime::did_you_mean::method_not_found(
                    "refinee", &how_name,
                ));
            };
            Ok(Value::package(Symbol::intern(&base)))
    }

        // `$obj.^mixin_base`: the type a role mixin was composed onto
        // (`(A | B) but R` -> Junction). MUGS::Util::StructureValidator's
        // `Optional.ACCEPTS` delegates to it.
        // Cost: O(k) + the base's `.WHAT`, k = number of mixin override keys.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_mixin_base(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            if let ValueView::Mixin(inner, mixins) = args[0].view() {
                return self.mixin_base_what(inner, mixins, &[]);
            }
            let how = self.dispatch_how(&args[0], &[])?;
            let how_name = match how.view() {
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => "Mu".to_string(),
            };
            Err(crate::runtime::did_you_mean::method_not_found(
                "mixin_base",
                &how_name,
            ))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_ver(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            if let Some(meta) = self.types.type_metadata.get(&name)
                && let Some(value) = meta.get("ver").cloned()
            {
                return Ok(Self::version_from_value(value));
            }
            // Core-setting language versions surface as plain Strs
            // (`Int.^ver.WHAT` is Str in Rakudo); only a declared
            // `:ver(...)` adverb (the type_metadata path above) is a
            // real Version object.
            if let Some(subset) = self.registry().subsets.get(&name) {
                return Ok(Value::str(subset.version.clone()));
            }
            if name == "Grammar" {
                return Ok(Value::str_from("6.e"));
            }
            // A bare `package` uses PackageHOW, which has no `.^ver` at all, so
            // `P.^ver` must still throw X::Method::NotFound ("absent by design").
            if matches!(
                self.registry().package_kinds.get(&name),
                Some(crate::ast::PackageKind::Package)
            ) {
                return Err(RuntimeError::meta_method_not_found("ver", &name));
            }
            // Core setting types report the language version they were
            // declared in (`Int.^ver` is v6.c). Checked before the class
            // registry so an add_method stub for a builtin doesn't turn
            // this into Mu.
            if Self::is_builtin_type(&name) {
                return Ok(Value::str_from("6.c"));
            }
            // A class/module/role/enum with no declared version: `.^ver` is
            // `Mu` (an undefined type object), not an error -- matching
            // Rakudo. Reached e.g. when the `:ver(...)` adverb is an
            // expression mutsu does not evaluate at registration
            // (`unit class C:ver($?DISTRIBUTION.meta<ver>)`), or a plain
            // unversioned declaration.
            // TODO: evaluate expression-form `:ver(...)` adverbs at class
            // registration and store the result in type_metadata.
            if self.registry().classes.contains_key(&name)
                || self.registry().roles.contains_key(&name)
                || self.registry().enum_types.contains_key(&name)
                || self.registry().package_kinds.contains_key(&name)
            {
                return Ok(Value::package(crate::symbol::Symbol::intern("Mu")));
            }
            Err(RuntimeError::meta_method_not_found("ver", &name))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_auth(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            // A bare `package` uses PackageHOW, which has no `.^auth`.
            if matches!(
                self.registry().package_kinds.get(&name),
                Some(crate::ast::PackageKind::Package)
            ) {
                return Err(RuntimeError::meta_method_not_found("auth", &name));
            }
            // A type with no declared `:auth` has an empty-string auth
            // (`class C {}; C.^auth` eq ""), so default to "" rather than
            // throwing -- same shape as `.^api` below.
            if let Some(value) = self
                .types
                .type_metadata
                .get(&name)
                .and_then(|meta| meta.get("auth").cloned())
            {
                return Ok(Value::str(value.to_string_value()));
            }
            Ok(Value::str(String::new()))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_api(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            // A bare `package` uses PackageHOW, which has no `.^api`.
            if matches!(
                self.registry().package_kinds.get(&name),
                Some(crate::ast::PackageKind::Package)
            ) {
                return Err(RuntimeError::meta_method_not_found("api", &name));
            }
            // A declared `:api(...)` is stored in type_metadata; a type with no
            // `:api` has an empty-string api in Rakudo (`class C {}; C.^api` eq
            // ""), so default to "" rather than throwing.
            if let Some(value) = self
                .types
                .type_metadata
                .get(&name)
                .and_then(|meta| meta.get("api").cloned())
            {
                return Ok(Value::str(value.to_string_value()));
            }
            Ok(Value::str(String::new()))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_isa(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // `.^isa` answers with an Int 1/0 (Rakudo surfaces the nqp
            // boolean directly), not a Bool.
            // Allow calling .^isa on an instance: use the instance's class.
            let class_name = match args[0].view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                ValueView::RakuAst(node) => node.class.printed_name().to_string(),
                // Concrete builtin values do not have a Package view, but
                // their dispatch owner chain carries the same nominal
                // ancestry as the corresponding type object.  Use that
                // chain instead of treating every concrete receiver as an
                // unrelated type.
                _ => self.mop_receiver_owner(&args[0]),
            };
            let other_name = match args[1].view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                ValueView::RakuAst(node) => node.class.printed_name().to_string(),
                _ => return Ok(Value::int(0)),
            };
            let is_same = class_name == other_name;
            if is_same {
                return Ok(Value::int(1));
            }
            let class_resolved = class_name;
            let other_resolved = other_name;
            if class_resolved.starts_with("RakuAST::")
                && other_resolved.starts_with("RakuAST::")
            {
                return Ok(Value::int(crate::rakuast::type_object_isa(
                    &class_resolved,
                    &other_resolved,
                ) as i64));
            }
            // Clone the base out per step so the registry read guard never spans
            // iterations (recursive read locks may deadlock).
            if let Some(mut base) = self
                .registry()
                .subsets
                .get(&class_resolved)
                .map(|s| s.base.clone())
            {
                loop {
                    if base == other_resolved {
                        return Ok(Value::int(1));
                    }
                    let Some(parent_base) =
                        self.registry().subsets.get(&base).map(|s| s.base.clone())
                    else {
                        break;
                    };
                    if parent_base == base {
                        break;
                    }
                    base = parent_base;
                }
            }
            let mro = self.class_mro(&class_resolved);
            Ok(Value::int(
                mro.iter().any(|p| p.as_str() == other_resolved) as i64
            ))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_mro(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let mut include_roles = false;
            let mut include_concretizations = false;
            for arg in &args[1..] {
                match arg.view() {
                    ValueView::Pair(k, v) if k == "roles" => {
                        include_roles = v.truthy();
                    }
                    ValueView::Pair(k, v) if k == "concretizations" => {
                        include_concretizations = v.truthy();
                    }
                    _ => {}
                }
            }
            if include_roles || include_concretizations {
                let mro = self.classhow_mro_with_roles(&args[0], include_concretizations)?;
                Ok(Value::array(mro))
            } else {
                let mro = self.classhow_mro_names_without_does_roles(&args[0]);
                let mut values = self.mro_names_to_values(mro)?;
                // The head of an MRO is the invocant's own type object
                // (`C.^mro[0] === C`, `$o.^mro[0] === $o.WHAT`). Naming it
                // by the class name is only equivalent while the name has
                // exactly one type object behind it — it does not for a
                // role that has been punned (`R.new`: the name `R` is the
                // role *group*, while the instance's type is the punned
                // class) nor for a role-mixed value (`(1 but R)`, whose
                // type is `Int+{R}`, not `Int`). Take the type object from
                // `.WHAT`, which already answers all three correctly.
                if !matches!(args[0].view(), ValueView::Package(_))
                    && let Some(head) = values.first_mut()
                {
                    *head = self.dispatch_what(&args[0], Vec::new())?;
                }
                Ok(Value::array(values))
            }
    }

        // `Metamodel::TypePretense`: the chain a role type object pretends
        // to belong to. Rakudo mixes it into the three role metaclasses
        // only, so a `ClassHOW`/`EnumHOW`/`SubsetHOW` receiver must keep
        // throwing X::Method::NotFound. Ask the metaobject itself (the same
        // shape the `trusts` arm above uses) rather than re-deriving the
        // taxonomy, so a new HOW kind cannot silently gain the method.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_pretending_to_be(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // The receiver may be the role group (`R`), an individual
            // candidate's declaration-site key, or a curried role
            // (`R[Int]`, which arrives as a `ParametricRole` rather than a
            // `Package`). All three carry TypePretense; a class/enum/subset
            // does not.
            let type_name = match args[0].view() {
                ValueView::ParametricRole { base_name, .. } => base_name.resolve(),
                _ => self.mop_receiver_owner(&args[0]),
            };
            let base = type_name
                .split_once('[')
                .map_or(type_name.as_str(), |(base, _)| base);
            if !self.is_role_type_name(base) {
                let how = self.dispatch_how(&args[0], &[])?;
                let how_name = match how.view() {
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => "Mu".to_string(),
                };
                return Err(crate::runtime::did_you_mean::method_not_found(
                    "pretending_to_be",
                    &how_name,
                ));
            }
            Ok(Value::array(
                crate::runtime::types::ROLE_PRETENDS_TO_BE
                    .iter()
                    .map(|n| Value::package(Symbol::intern(n)))
                    .collect(),
            ))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_archetypes(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let invocant_name = self.mop_receiver_owner(&args[0]);
            let base_name = invocant_name
                .split_once('[')
                .map(|(base, _)| base)
                .unwrap_or(invocant_name.as_str());
            let is_role = self.registry().roles.contains_key(base_name);
            let is_subset = self.registry().subsets.contains_key(base_name);
            // A coercion type (`Str(Int)`) carries parens in its name. Use
            // the strict form check — a bare `contains('(')` also fires on
            // parens embedded in a where-clause of a `T{K}` key-typed hash
            // (`Associative[Str{subset ... where any("a", "b")}]`).
            let is_coercive = crate::runtime::types::is_coercion_constraint(&invocant_name);
            // A definite type (`Int:D` / `Int:U`) wraps its base type
            // (rakudo: nominal=False, nominalizable=True, definite=True).
            let is_definite = invocant_name.ends_with(":D") || invocant_name.ends_with(":U");
            let mut attrs = HashMap::new();
            attrs.insert("composable".to_string(), Value::truth(is_role));
            // Classes, enums, and roles are nominal; subsets, coercion
            // types, and definite types are not (JSON::Unmarshal's
            // ClassLike subset — rakudo reports roles as nominal too).
            attrs.insert(
                "nominal".to_string(),
                Value::truth(!is_subset && !is_coercive && !is_definite),
            );
            // Subsets, coercion types, and definite types can be
            // nominalized (^nominalize).
            attrs.insert(
                "nominalizable".to_string(),
                Value::truth(is_subset || is_coercive || is_definite),
            );
            attrs.insert("coercive".to_string(), Value::truth(is_coercive));
            attrs.insert("definite".to_string(), Value::truth(is_definite));
            Ok(Value::make_instance(
                Symbol::intern("Perl6::Metamodel::Archetypes"),
                attrs,
            ))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_nominalize(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let invocant_name = self.mop_receiver_owner(&args[0]);
            let nominal = self.nominalize_type_name(&invocant_name);
            Ok(Value::package(Symbol::intern(&nominal)))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_mro_unhidden(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let mut include_roles = false;
            let mut include_concretizations = false;
            for arg in &args[1..] {
                match arg.view() {
                    ValueView::Pair(k, v) if k == "roles" => {
                        include_roles = v.truthy();
                    }
                    ValueView::Pair(k, v) if k == "concretizations" => {
                        include_concretizations = v.truthy();
                    }
                    _ => {}
                }
            }
            if include_roles || include_concretizations {
                let mro = self.classhow_mro_with_roles(&args[0], include_concretizations)?;
                let filtered = self.filter_mro_unhidden(&args[0], mro);
                Ok(Value::array(filtered))
            } else {
                let mro = self.classhow_mro_unhidden_names(&args[0]);
                Ok(Value::array(self.mro_names_to_values(mro)?))
            }
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_can(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let invocant = &args[args.len() - 2];
            // The method name is always the last argument. When called via ^can,
            // the args may be [Package, target, method_name] due to Package insertion.
            let method_name = args.last().unwrap().to_string_value();
            let results = self.collect_can_methods(invocant, &method_name);
            Ok(Value::array(results))
    }
}
