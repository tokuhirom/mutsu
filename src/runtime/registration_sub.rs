use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn default_check_constraint_base(constraint: &str) -> &str {
        let mut end = constraint.len();
        for (idx, ch) in constraint.char_indices() {
            if ch == '[' || ch == '(' || ch == ':' {
                end = idx;
                break;
            }
        }
        &constraint[..end]
    }

    fn static_default_value_for_typecheck(&mut self, expr: &Expr) -> Option<Value> {
        match expr {
            Expr::Literal(v) => Some(v.clone()),
            Expr::AnonSub { .. } | Expr::AnonSubParams { .. } => {
                self.eval_block_value(&[Stmt::Expr(expr.clone())]).ok()
            }
            _ => None,
        }
    }

    fn validate_static_default_typechecks(
        &mut self,
        param_defs: &[ParamDef],
    ) -> Result<(), RuntimeError> {
        for pd in param_defs {
            let Some(constraint) = &pd.type_constraint else {
                continue;
            };
            let base_constraint = Self::default_check_constraint_base(constraint);
            if !self.has_type(base_constraint) {
                continue;
            }
            let Some(default_expr) = &pd.default else {
                continue;
            };
            let Some(default_value) = self.static_default_value_for_typecheck(default_expr) else {
                continue;
            };
            if !self.type_matches_value(constraint, &default_value) {
                return Err(RuntimeError::new(format!(
                    "X::Parameter::Default::TypeCheck: Type check failed for default value of parameter '{}'; expected {}, got {}",
                    pd.name,
                    constraint,
                    super::value_type_name(&default_value)
                )));
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        return_type: Option<&String>,
        associativity: Option<&String>,
        body: &[Stmt],
        multi: bool,
        is_rw: bool,
        is_raw: bool,
        is_test_assertion: bool,
        supersede: bool,
        custom_traits: &[String],
    ) -> Result<(), RuntimeError> {
        let is_method_value_decl = custom_traits.iter().any(|t| t == "__mutsu_method_decl");
        let allow_redeclare = supersede || is_method_value_decl;
        Self::validate_callable_param_return_redeclaration(param_defs)?;
        if let Some(spec) = return_type
            && self.is_definite_return_spec(spec)
            && Self::body_contains_non_nil_return(body)
        {
            return Err(Self::malformed_return_value_compile_error());
        }
        // Auto-detect @_ / %_ usage for subs without explicit signatures
        let (effective_param_defs, empty_sig) = if param_defs.is_empty() && params.is_empty() {
            let (use_positional, use_named) = Self::auto_signature_uses(body);
            let mut defs = Vec::new();
            if use_positional {
                defs.push(ParamDef {
                    name: "@_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            if use_named {
                defs.push(ParamDef {
                    name: "%_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            // If neither @_ nor %_ is used, this is a true empty signature
            let is_empty = defs.is_empty();
            (defs, is_empty)
        } else {
            (param_defs.to_vec(), false)
        };
        self.validate_static_default_typechecks(&effective_param_defs)?;
        let new_def = FunctionDef {
            package: Symbol::intern(&self.current_package),
            name: Symbol::intern(name),
            params: params.to_vec(),
            param_defs: effective_param_defs,
            body: body.to_vec(),
            is_test_assertion,
            is_rw,
            is_raw,
            is_method: false,
            empty_sig,
            return_type: return_type.cloned(),
        };
        let single_key = format!("{}::{}", self.current_package, name);
        let multi_prefix = format!("{}::{}/", self.current_package, name);
        let single_key_sym = Symbol::intern(&single_key);
        let has_single = self.functions.contains_key(&single_key_sym);
        let has_multi = self
            .functions
            .keys()
            .any(|k| k.resolve().starts_with(&multi_prefix));
        let has_proto = self.proto_subs.contains(&single_key);
        let allow_lexical_shadow = self.block_scope_depth > 0;
        let code_var_key = format!("&{}", name);
        if let Some(existing) = self.env.get(&code_var_key) {
            // Mixin values in &name come from trait_mod and should not block registration
            if !matches!(existing, Value::Mixin(..))
                && !allow_lexical_shadow
                && !is_method_value_decl
            {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared as code variable",
                    name
                )));
            }
        }
        if let Some(existing) = self.functions.get(&single_key_sym) {
            let same = existing.package == new_def.package
                && existing.name == new_def.name
                && existing.params == new_def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", new_def.param_defs)
                && format!("{:?}", existing.body) == format!("{:?}", new_def.body);
            if same {
                let callable_key =
                    format!("__mutsu_callable_id::{}::{}", self.current_package, name);
                self.env.insert(
                    callable_key,
                    Value::Int(crate::value::next_instance_id() as i64),
                );
                return Ok(());
            }
            let same_signature = existing.package == new_def.package
                && existing.name == new_def.name
                && existing.params == new_def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", new_def.param_defs);
            if body.is_empty() && same_signature {
                let callable_key =
                    format!("__mutsu_callable_id::{}::{}", self.current_package, name);
                self.env.insert(
                    callable_key,
                    Value::Int(crate::value::next_instance_id() as i64),
                );
                return Ok(());
            }
        }
        let existing_is_stub = self
            .functions
            .get(&single_key_sym)
            .is_some_and(|existing| Self::is_stub_routine_body(&existing.body));
        if multi {
            if has_single && !has_proto && !allow_redeclare && !allow_lexical_shadow {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared as non-multi",
                    name
                )));
            }
        } else if !allow_redeclare {
            // Defining a plain sub when multi candidates exist (without proto) is always
            // an error, even in inner scopes, because it creates an ambiguous dispatch.
            if has_multi && !has_proto {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared",
                    name
                )));
            }
            if !allow_lexical_shadow && has_single && !existing_is_stub {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared",
                    name
                )));
            }
        }
        let def = new_def;
        if let Some(assoc) = associativity {
            self.operator_assoc.insert(name.to_string(), assoc.clone());
            self.operator_assoc
                .insert(format!("{}::{}", self.current_package, name), assoc.clone());
        }
        if multi {
            let arity = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
                .count();
            let type_sig: Vec<&str> = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
                .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                .collect();
            let has_types = type_sig.iter().any(|t| *t != "Any");
            if has_types {
                let typed_fq = format!(
                    "{}::{}/{}:{}",
                    self.current_package,
                    name,
                    arity,
                    type_sig.join(",")
                );
                match self.functions.entry(Symbol::intern(&typed_fq)) {
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(def.clone());
                    }
                    std::collections::hash_map::Entry::Occupied(_) => {
                        let mut idx = 1usize;
                        loop {
                            let key = format!("{}__m{}", typed_fq, idx);
                            if let std::collections::hash_map::Entry::Vacant(entry) =
                                self.functions.entry(Symbol::intern(&key))
                            {
                                entry.insert(def.clone());
                                break;
                            }
                            idx += 1;
                        }
                    }
                }
            }
            let fq = format!("{}::{}/{}", self.current_package, name, arity);
            if !has_types || name == "trait_mod:<is>" {
                match self.functions.entry(Symbol::intern(&fq)) {
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(def);
                    }
                    std::collections::hash_map::Entry::Occupied(_) => {
                        let mut idx = 1usize;
                        loop {
                            let key = format!("{}__m{}", fq, idx);
                            if let std::collections::hash_map::Entry::Vacant(entry) =
                                self.functions.entry(Symbol::intern(&key))
                            {
                                entry.insert(def);
                                break;
                            }
                            idx += 1;
                        }
                    }
                }
            } else {
                self.functions.entry(Symbol::intern(&fq)).or_insert(def);
            }
        } else {
            let fq = format!("{}::{}", self.current_package, name);
            self.functions.insert(Symbol::intern(&fq), def);
        }
        let callable_key = format!("__mutsu_callable_id::{}::{}", self.current_package, name);
        self.env.insert(
            callable_key,
            Value::Int(crate::value::next_instance_id() as i64),
        );
        if is_method_value_decl {
            let sub_val = Value::make_sub(
                Symbol::intern(&self.current_package),
                Symbol::intern(name),
                params.to_vec(),
                param_defs.to_vec(),
                body.to_vec(),
                is_rw,
                self.env.clone(),
            );
            self.env.insert(format!("&{}", name), sub_val);
            self.env
                .insert(format!("__mutsu_method_value::{}", name), Value::Bool(true));
        }
        // Apply custom trait_mod:<is> for each non-builtin trait (only if trait_mod:<is> is defined)
        if !custom_traits.is_empty()
            && (self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>"))
        {
            for trait_name in custom_traits {
                let sub_val = Value::make_sub(
                    Symbol::intern(&self.current_package),
                    Symbol::intern(name),
                    params.to_vec(),
                    param_defs.to_vec(),
                    body.to_vec(),
                    is_rw,
                    self.env.clone(),
                );
                let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                let result = self.call_function("trait_mod:<is>", vec![sub_val, named_arg])?;
                // If the trait_mod returned a modified sub (e.g. with CALL-ME mixed in),
                // store it in the env so function dispatch can find it.
                if matches!(result, Value::Mixin(..)) {
                    self.env.insert(format!("&{}", name), result);
                }
            }
        }
        Ok(())
    }

    pub(crate) fn register_token_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
    ) {
        let def = FunctionDef {
            package: Symbol::intern(&self.current_package),
            name: Symbol::intern(name),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
            is_test_assertion: false,
            is_rw: false,
            is_raw: false,
            is_method: false,
            empty_sig: false,
            return_type: None,
        };
        self.insert_token_def(name, def, multi);
    }

    pub(crate) fn register_proto_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let key = format!("{}::{}", self.current_package, name);
        if self.functions.contains_key(&Symbol::intern(&key)) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared",
                name
            )));
        }
        if self.proto_subs.contains(&key) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared as proto",
                name
            )));
        }
        self.proto_subs.insert(key);
        let fq = format!("{}::{}", self.current_package, name);
        self.proto_functions.insert(
            Symbol::intern(&fq),
            FunctionDef {
                package: Symbol::intern(&self.current_package),
                name: Symbol::intern(name),
                params: params.to_vec(),
                param_defs: param_defs.to_vec(),
                body: body.to_vec(),
                is_test_assertion: false,
                is_rw: false,
                is_raw: false,
                is_method: false,
                empty_sig: false,
                return_type: None,
            },
        );
        Ok(())
    }

    /// Register a sub under GLOBAL:: (used for `is export` trait).
    #[allow(dead_code)]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl_as_global(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        return_type: Option<&String>,
        associativity: Option<&String>,
        body: &[Stmt],
        multi: bool,
        is_rw: bool,
        is_raw: bool,
        is_test_assertion: bool,
        supersede: bool,
    ) -> Result<(), RuntimeError> {
        Self::validate_callable_param_return_redeclaration(param_defs)?;
        if let Some(spec) = return_type
            && self.is_definite_return_spec(spec)
            && Self::body_contains_non_nil_return(body)
        {
            return Err(Self::malformed_return_value_compile_error());
        }
        // Auto-detect @_ / %_ usage for subs without explicit signatures
        let (effective_param_defs, empty_sig) = if param_defs.is_empty() && params.is_empty() {
            let (use_positional, use_named) = Self::auto_signature_uses(body);
            let mut defs = Vec::new();
            if use_positional {
                defs.push(ParamDef {
                    name: "@_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            if use_named {
                defs.push(ParamDef {
                    name: "%_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            let is_empty = defs.is_empty();
            (defs, is_empty)
        } else {
            (param_defs.to_vec(), false)
        };
        let def = FunctionDef {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern(name),
            params: params.to_vec(),
            param_defs: effective_param_defs,
            body: body.to_vec(),
            is_test_assertion,
            is_rw,
            is_raw,
            is_method: false,
            empty_sig,
            return_type: return_type.cloned(),
        };
        let single_key = format!("GLOBAL::{}", name);
        let single_key_sym = Symbol::intern(&single_key);
        let multi_prefix = format!("GLOBAL::{}/", name);
        let has_single = self.functions.contains_key(&single_key_sym);
        let has_multi = self
            .functions
            .keys()
            .any(|k| k.resolve().starts_with(&multi_prefix));
        let has_proto = self.proto_subs.contains(&single_key);
        if let Some(assoc) = associativity {
            self.operator_assoc.insert(name.to_string(), assoc.clone());
            self.operator_assoc
                .insert(format!("GLOBAL::{}", name), assoc.clone());
        }
        if let Some(existing) = self.functions.get(&single_key_sym) {
            let same = existing.package == def.package
                && existing.name == def.name
                && existing.params == def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", def.param_defs)
                && format!("{:?}", existing.body) == format!("{:?}", def.body);
            if same {
                return Ok(());
            }
            let same_signature = existing.package == def.package
                && existing.name == def.name
                && existing.params == def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", def.param_defs);
            if body.is_empty() && same_signature {
                return Ok(());
            }
        }
        let existing_is_stub = self
            .functions
            .get(&single_key_sym)
            .is_some_and(|existing| Self::is_stub_routine_body(&existing.body));
        if multi {
            if has_single && !has_proto && !supersede {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared as non-multi",
                    name
                )));
            }
            let arity = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
                .count();
            let type_sig: Vec<&str> = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
                .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                .collect();
            let has_types = type_sig.iter().any(|t| *t != "Any");
            if has_types {
                let typed_fq = format!("GLOBAL::{}/{}:{}", name, arity, type_sig.join(","));
                self.functions
                    .insert(Symbol::intern(&typed_fq), def.clone());
            }
            let fq = format!("GLOBAL::{}/{}", name, arity);
            if !has_types {
                match self.functions.entry(Symbol::intern(&fq)) {
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(def);
                    }
                    std::collections::hash_map::Entry::Occupied(_) => {
                        let mut idx = 1usize;
                        loop {
                            let key = format!("{}__m{}", fq, idx);
                            if let std::collections::hash_map::Entry::Vacant(entry) =
                                self.functions.entry(Symbol::intern(&key))
                            {
                                entry.insert(def);
                                break;
                            }
                            idx += 1;
                        }
                    }
                }
            } else {
                self.functions.entry(Symbol::intern(&fq)).or_insert(def);
            }
        } else {
            if has_multi && !has_proto && !supersede {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared",
                    name
                )));
            }
            if has_single && !supersede && !existing_is_stub {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared",
                    name
                )));
            }
            let fq = format!("GLOBAL::{}", name);
            self.functions.insert(Symbol::intern(&fq), def);
        }
        let callable_key = format!("__mutsu_callable_id::GLOBAL::{}", name);
        self.env.insert(
            callable_key,
            Value::Int(crate::value::next_instance_id() as i64),
        );
        Ok(())
    }

    /// Register a proto sub under GLOBAL:: (used for `is export` trait).
    pub(crate) fn register_proto_decl_as_global(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let key = format!("GLOBAL::{}", name);
        if self.functions.contains_key(&Symbol::intern(&key)) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared",
                name
            )));
        }
        if self.proto_subs.contains(&key) {
            if self.current_package == "GLOBAL" {
                // `is export` on a GLOBAL proto hits both local/global registration paths.
                return Ok(());
            }
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared as proto",
                name
            )));
        }
        self.proto_subs.insert(key.clone());
        self.proto_functions.insert(
            Symbol::intern(&key),
            FunctionDef {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(name),
                params: params.to_vec(),
                param_defs: param_defs.to_vec(),
                body: body.to_vec(),
                is_test_assertion: false,
                is_rw: false,
                is_raw: false,
                is_method: false,
                empty_sig: false,
                return_type: None,
            },
        );
        Ok(())
    }

    pub(crate) fn register_proto_token_decl(&mut self, name: &str) {
        let key = format!("{}::{}", self.current_package, name);
        self.proto_tokens.insert(key);
    }

    pub(crate) fn register_enum_decl(
        &mut self,
        name: &str,
        variants: &[(String, Option<Expr>)],
        is_export: bool,
    ) -> Result<Value, RuntimeError> {
        // Handle dynamic enum body: `enum Stuff (@variable)`
        let expanded;
        let variants: &[(String, Option<Expr>)] =
            if variants.len() == 1 && variants[0].0 == "__DYNAMIC__" && variants[0].1.is_some() {
                let expr = variants[0].1.as_ref().unwrap();
                let v = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                let items: Vec<Value> = match &v {
                    Value::Array(items, _) => items.as_ref().clone(),
                    Value::Slip(items) => items.as_ref().clone(),
                    _ => vec![v.clone()],
                };
                expanded = items
                    .iter()
                    .map(|item| (item.to_str_context(), None::<Expr>))
                    .collect::<Vec<_>>();
                &expanded[..]
            } else {
                variants
            };
        let mut enum_variants: Vec<(String, i64, Option<Symbol>)> = Vec::new();
        let mut next_value: i64 = 0;
        // Track whether this is a string-valued enum (for string auto-increment)
        let mut last_str_value: Option<String> = None;
        for (key, value_expr) in variants {
            if let Some(expr) = value_expr {
                let v = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                match v {
                    Value::Int(i) => {
                        enum_variants.push((key.clone(), i, None));
                        next_value = i + 1;
                        last_str_value = None;
                    }
                    Value::Bool(b) => {
                        let i = if b { 1 } else { 0 };
                        enum_variants.push((key.clone(), i, None));
                        next_value = i + 1;
                        last_str_value = None;
                    }
                    Value::Str(s) => {
                        // String-valued enum variant
                        let sv = Symbol::intern(&s);
                        enum_variants.push((key.clone(), next_value, Some(sv)));
                        last_str_value = Some(s.to_string());
                        next_value += 1;
                    }
                    _ => {
                        enum_variants.push((key.clone(), next_value, None));
                        next_value += 1;
                        last_str_value = None;
                    }
                }
            } else if let Some(ref prev_str) = last_str_value {
                // String auto-increment: next string value after the previous one
                let incremented = string_increment(prev_str);
                let sv = Symbol::intern(&incremented);
                enum_variants.push((key.clone(), next_value, Some(sv)));
                last_str_value = Some(incremented);
                next_value += 1;
            } else {
                enum_variants.push((key.clone(), next_value, None));
                next_value += 1;
            }
        }
        // Check for mixed int/string enum values (forbidden in Raku)
        let has_str = enum_variants.iter().any(|(_, _, sv)| sv.is_some());
        let has_int = enum_variants.iter().any(|(_, _, sv)| sv.is_none());
        if has_str && has_int {
            return Err(RuntimeError::new(
                "Incompatible MROs in P6opaque rebless for types Str",
            ));
        }
        let is_anonymous = name.is_empty();
        let enum_type_name = if is_anonymous { "__ANON_ENUM__" } else { name };
        self.enum_types
            .insert(enum_type_name.to_string(), enum_variants.clone());
        if !is_anonymous {
            self.env
                .insert(name.to_string(), Value::Package(Symbol::intern(name)));
            // Also register with fully-qualified package name
            if self.current_package != "GLOBAL" {
                self.env.insert(
                    format!("{}::{}", self.current_package, name),
                    Value::Package(Symbol::intern(name)),
                );
            }
        }
        for (index, (key, val, sv)) in enum_variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: Symbol::intern(enum_type_name),
                key: Symbol::intern(key),
                value: *val,
                index,
                str_value: *sv,
            };
            if !is_anonymous {
                self.env
                    .insert(format!("{}::{}", name, key), enum_val.clone());
                // Also register with fully-qualified package name
                if self.current_package != "GLOBAL" {
                    self.env.insert(
                        format!("{}::{}::{}", self.current_package, name, key),
                        enum_val.clone(),
                    );
                }
            }
            // Also register bare variant with package prefix for import lookup
            if self.current_package != "GLOBAL" {
                self.env.insert(
                    format!("{}::{}", self.current_package, key),
                    enum_val.clone(),
                );
            }
            self.env.insert(key.clone(), enum_val);
        }
        // Register exports if `is export`
        if is_export && !is_anonymous {
            let pkg = self.current_package.clone();
            for (key, _, _) in &enum_variants {
                self.register_exported_var(pkg.clone(), key.clone(), vec!["DEFAULT".to_string()]);
            }
        }

        // For anonymous enums, return a Map (Hash) of key => value pairs
        if is_anonymous {
            let mut map = HashMap::new();
            for (key, val, sv) in &enum_variants {
                if let Some(s) = sv {
                    map.insert(key.clone(), Value::str(s.resolve()));
                } else {
                    map.insert(key.clone(), Value::Int(*val));
                }
            }
            Ok(Value::hash(map))
        } else {
            Ok(Value::Nil)
        }
    }
}

/// Increment a string value (Raku string increment semantics).
/// For single-char strings, increments the last character.
/// "x" -> "y", "z" -> "aa", "Z" -> "AA"
fn string_increment(s: &str) -> String {
    if s.is_empty() {
        return "a".to_string();
    }
    let mut chars: Vec<char> = s.chars().collect();
    let mut i = chars.len();
    loop {
        if i == 0 {
            // All chars wrapped around, prepend a new char
            let first = chars[0];
            let prefix = if first.is_ascii_uppercase() { 'A' } else { 'a' };
            chars.insert(0, prefix);
            break;
        }
        i -= 1;
        let c = chars[i];
        if c.is_ascii_lowercase() {
            if c == 'z' {
                chars[i] = 'a';
                // carry
            } else {
                chars[i] = (c as u8 + 1) as char;
                break;
            }
        } else if c.is_ascii_uppercase() {
            if c == 'Z' {
                chars[i] = 'A';
                // carry
            } else {
                chars[i] = (c as u8 + 1) as char;
                break;
            }
        } else if c.is_ascii_digit() {
            if c == '9' {
                chars[i] = '0';
                // carry
            } else {
                chars[i] = (c as u8 + 1) as char;
                break;
            }
        } else {
            // Non-alphanumeric: just increment codepoint
            chars[i] = char::from_u32(c as u32 + 1).unwrap_or(c);
            break;
        }
    }
    chars.into_iter().collect()
}
