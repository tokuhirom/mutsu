//! `Metamodel::*HOW` methods, part 3 (ADR-11276 slice 3G): one Interpreter method per
//! metamethod. The rows that reach them are `method_table/ctors_mop/class_how.rs`.

use super::methods_classhow_dispatch::{mop_absent_method};
use super::*;
use crate::meta_ns::MetaNs;

impl Interpreter {
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_declares_method(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // `Metamodel::MethodContainer.declares_method` is a local
            // declaration probe: unlike `.^lookup`/`.^find_method`, it
            // must not walk the target type's MRO.  Red uses this to
            // decide whether it should wrap a class's existing BUILD or
            // TWEAK method while composing its model roles.
            let target = &args[0];
            let Some(method_arg) = args.last() else {
                return Err(RuntimeError::new(
                    "declares_method requires a target type and method name",
                ));
            };
            let method_name = method_arg.to_string_value();
            let how = self.dispatch_how(target, &[])?;
            let how_name = match how.view() {
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => "Mu".to_string(),
            };
            let supported = matches!(
                how_name.as_str(),
                "Perl6::Metamodel::ClassHOW"
                    | "Perl6::Metamodel::GrammarHOW"
                    | "Perl6::Metamodel::EnumHOW"
            ) || (self.is_metamodel_how_class(&how_name)
                && self
                    .registry()
                    .classes
                    .get(&how_name)
                    .is_some_and(|class_def| {
                        class_def.mro.iter().any(|parent| {
                            matches!(
                                parent.as_str(),
                                "Metamodel::ClassHOW"
                                    | "Metamodel::GrammarHOW"
                                    | "Perl6::Metamodel::ClassHOW"
                                    | "Perl6::Metamodel::GrammarHOW"
                            )
                        })
                    }));
            if !supported {
                return Err(crate::runtime::did_you_mean::method_not_found(
                    "declares_method",
                    &how_name,
                ));
            }

            let owner = self.mop_receiver_owner(target);
            let name = Symbol::intern(&method_name);
            let registry = self.registry();
            // `is_my` also represents a `submethod` in the canonical
            // table.  Submethods are declarations for this probe, while
            // lexical `my method`s never enter that table at all.
            let user_method = registry
                .user_method_public_presence(Symbol::intern(&owner), name)
                == Some(true);
            let proto_method = registry.method_entry_proto(&owner, &method_name).is_some();
            let public_accessor =
                registry.accessor_is_public_sym(Symbol::intern(&owner), name) == Some(true);
            let class_level_accessor = registry.classes.get(&owner).is_some_and(|class_def| {
                class_def.class_level_attrs.contains_key(&method_name)
            });
            let native_method = registry
                .classes
                .get(&owner)
                .is_some_and(|class_def| class_def.native_methods.contains(&method_name));
            let grammar_token = registry.token_defs.contains_key(&Symbol::intern(
                crate::qualified::qualified_text(&owner, &method_name).as_str(),
            ));

            Ok(Value::int(
                (user_method
                    || proto_method
                    || public_accessor
                    || class_level_accessor
                    || native_method
                    || grammar_token) as i64,
            ))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_does(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let invocant = &args[args.len() - 2];
            let role_arg = args.last().unwrap();
            // Handle ParametricRole directly to compare type args properly
            if let ValueView::ParametricRole {
                base_name,
                type_args,
            } = role_arg.view()
            {
                let base = base_name.resolve();
                if let ValueView::Mixin(_, mixins) = invocant.view() {
                    let key = MetaNs::RoleTypeargs.owned_key_for_str(&base);
                    let has_role = invocant.does_check(&base);
                    let args_match = if let Some(ValueView::Array(actual_args, ..)) =
                        mixins.get(&key).map(Value::view)
                    {
                        actual_args.len() == type_args.len()
                            && actual_args
                                .iter()
                                .zip(type_args.iter())
                                .all(|(a, e)| self.parametric_arg_subtypes(a, e))
                    } else {
                        type_args.is_empty()
                    };
                    return Ok(Value::truth(has_role && args_match));
                }
                return Ok(Value::truth(self.type_matches_value(
                    &format!(
                            "{}[{}]",
                            base,
                            type_args
                                .iter()
                                .map(|a| a.to_string_value())
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    invocant,
                )));
            }
            let type_name = match role_arg.view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => role_arg.to_string_value(),
            };
            Ok(Value::truth(self.type_matches_value(&type_name, invocant)))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_lookup(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // See `find_method`: prefer the mixin value over its Package.
            let invocant = args
                .iter()
                .find(|a| matches!(a.view(), ValueView::Mixin(..)))
                .unwrap_or(&args[0]);
            // Method name is always the last argument; when ^lookup is called on
            // a concrete value the Package is prepended and the original value
            // sits in between.
            let method_name = args.last().unwrap().to_string_value();
            Ok(self
                .classhow_lookup(invocant, &method_name)
                .unwrap_or_else(mop_absent_method))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_find_method(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // `$mixin.^find_method(...)` prepends the Package; the mixin
            // value itself (which carries the mixed-in roles) sits after it.
            let invocant = args
                .iter()
                .find(|a| matches!(a.view(), ValueView::Mixin(..)))
                .unwrap_or(&args[0]);
            // The method name is the last *positional* argument: calling
            // `$obj.^find_method('v')` on a concrete value prepends the Package and
            // leaves the instance in between (so `args[1]` is not the name), while
            // `.^find_method('foo', :no_fallback)` trails an adverb after it.
            let Some(name_arg) = args
                .iter()
                .rev()
                .find(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            else {
                return Ok(mop_absent_method());
            };
            let method_name = name_arg.to_string_value();
            Ok(self
                .classhow_find_method(invocant, &method_name)
                .unwrap_or_else(mop_absent_method))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_parameterize(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // `$type.^parameterize($T, ...)` — the metamodel form of the
            // `Type[$T]` postcircumfix parameterization. Build the same
            // `Base[Arg,...]` package name that `vm_var_index_ops.rs` produces
            // for the `[ ]` syntax, so `Set.^parameterize(Str)` and `Set[Str]`
            // yield an identical parameterized type object.
            // Parameterizing is always relative to the *generic base*, not
            // to a previously-curried spelling.  In particular, the MOP
            // permits a caller to reuse one `$type` lexical for successive
            // parameterizations (`Set[Str].^parameterize(Int())` means
            // `Set[Int()]`, not the nonexistent `Set[Str][Int()]`).
            let owner = self.mop_receiver_owner(&args[0]);
            let base = owner
                .split_once('[')
                .map(|(base, _)| base)
                .unwrap_or(owner.as_str());
            // A role is parameterized by its argument *values*, exactly as
            // `R[$v]` is (`Value::parametric_role`): spelling them into a
            // package name lost them, so `R.^parameterize($signature)`
            // named a role `R[Int $a]` whose parameter matched no
            // candidate (Badger's `does SignatureOverload.^parameterize($sig)`).
            if self.is_role(base) {
                let type_args: Vec<Value> = args[1..]
                    .iter()
                    .filter(|a| {
                        !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
                    })
                    .cloned()
                    .collect();
                return Ok(if type_args.is_empty() {
                    Value::package(Symbol::intern(base))
                } else {
                    Value::parametric_role(Symbol::intern(base), type_args)
                });
            }
            let param_args = args[1..]
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                .map(|v| match v.view() {
                    ValueView::Package(name) => name.resolve(),
                    _ => {
                        let s = v.to_string_value();
                        s.trim_start_matches('(').trim_end_matches(')').to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join(",");
            Ok(Value::package(Symbol::intern(&format!(
                "{}[{}]",
                base, param_args
            ))))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_coerce(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let target_constraint = self.mop_receiver_owner(&args[0]);
            let original = args[1].clone();
            let parse_coercion = |constraint: &str| -> Option<(String, Option<String>)> {
                if !constraint.ends_with(')') || constraint.contains('[') {
                    return None;
                }
                let open = constraint.find('(')?;
                if open == 0 {
                    return None;
                }
                let target = constraint[..open].to_string();
                let source = &constraint[open + 1..constraint.len() - 1];
                let source = if source.is_empty() {
                    None
                } else {
                    Some(source.to_string())
                };
                Some((target, source))
            };
            if let Some((_target, source)) = parse_coercion(&target_constraint)
                && let Some(src) = source.as_ref()
                && !self.type_matches_value(src, &original)
            {
                return Err(super::types::coerce_impossible_error(
                    &target_constraint,
                    &original,
                ));
            }
            let coerced =
                self.try_coerce_value_for_constraint(&target_constraint, original.clone())?;
            if let Some((target, _)) = parse_coercion(&target_constraint)
                && !self.type_matches_value(&target, &coerced)
            {
                return Err(super::types::coerce_impossible_error(
                    &target_constraint,
                    &original,
                ));
            }
            Ok(coerced)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_add_role(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let class_name = match args[0].view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                _ => {
                    return Err(RuntimeError::new("add_role target must be a type object"));
                }
            };
            // A role declaration expression (`role :: { ... }`, `role R
            // { ... }`) evaluates to its individual candidate's site key;
            // composition is keyed by the role group, so normalise first
            // like every other consumer of a role type object.
            let role = self.normalize_role_type_object(&args[1]);
            let role_name = super::registration_class::type_value_name(&role);
            self.add_role_to_class(&class_name, &role_name)?;
            Ok(Value::NIL)
    }

        // `$role.^set_body_block(&block)` on a `ParametricRoleHOW`-built
        // role. Only a role has a body block; Rakudo's class metaclasses
        // have no such method.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_set_body_block(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let ValueView::Package(name) = args[0].view() else {
                unreachable!("guarded above");
            };
            Ok(self.set_role_body_block(&name.resolve(), args[1].clone()))
    }

    /// Whether `set_body_block` applies to these arguments (the arm's own guard).
    // Cost: O(1).
    pub(crate) fn mop_set_body_block_applies(&self, args: &[Value]) -> bool {
        matches!(args[0].view(), ValueView::Package(name) if self.registry().roles.contains_key(&name.resolve()))
    }
}
