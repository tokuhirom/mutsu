//! A class's BUILDPLAN, in Rakudo's encoding (ADR-14162).
//!
//! Rakudo initializes an object by running the class's BUILDPLAN, a list of
//! tasks `Metamodel::BUILDPLAN.create_BUILDPLAN` derives when the class
//! composes. The plan is part of the meta-object protocol: a HOW mixin can
//! override `create_BUILDPLAN`, read the plan through
//! `nqp::getattr($how, Metamodel::ClassHOW, '@!BUILDPLAN')` and bind a
//! rewritten one (AttrX::Mooish puts its lazy-attribute installer where an
//! attribute's init task was, #14162).
//!
//! mutsu constructs natively (`NativeCtorPlan`), so the task list here is a
//! second view of the same class shape, built on demand. It is stored only
//! when the MOP asks for that: `create_BUILDPLAN` records the plan it built,
//! and a bind records a replacement, after which construction runs the plan
//! (`buildplan_run.rs`). A class nobody touched constructs natively, as before.
//!
//! A task is either a `Code` (a `BUILD`/`TWEAK` submethod, or a block a module
//! put there), called with the instance and the named arguments, or an nqp
//! list `[code, type, '$!name', payload...]`. The codes are those of the
//! Rakudo whose version `$*RAKU.compiler.version` reports:
//!
//! | code | task | payload |
//! | --- | --- | --- |
//! | 0 / 1 / 2 / 3 | set an object / `int` / `num` / `str` attribute from the named argument | the argument name |
//! | 800 | die unless the attribute was set (`is required`) | `1`, or the reason |
//! | 400 / 401 / 402 / 403 | set the default unless the attribute was set | the value, or a `Code` |
//! | 1000 | nothing (the attribute keeps its container) | |

use super::*;

/// The task codes, as Rakudo v2021.12 and later number them.
pub(crate) mod task_code {
    pub(crate) const INIT_OBJ: i64 = 0;
    pub(crate) const INIT_STR: i64 = 3;
    pub(crate) const DEFAULT_OBJ: i64 = 400;
    pub(crate) const DEFAULT_STR: i64 = 403;
    pub(crate) const REQUIRED: i64 = 800;
    pub(crate) const NOOP: i64 = 1000;
}

/// What the MOP stored for one class: the plans `create_BUILDPLAN` built or a
/// module bound, and whether a bind replaced the derived plan.
#[derive(Debug, Clone, Default)]
pub(crate) struct BuildPlanState {
    pub(crate) plan: Option<Value>,
    pub(crate) all_plan: Option<Value>,
    /// `all_plan` was bound by a module, not derived by `create_BUILDPLAN`.
    pub(crate) all_plan_bound: bool,
    pub(crate) replaced: bool,
}

/// The native kind of an attribute's declared type: `None` for an object
/// attribute, else the offset of its init (`0 + k`) and default (`400 + k`)
/// task codes.
// Cost: O(1).
fn native_kind(type_constraint: Option<&str>) -> i64 {
    match type_constraint {
        Some(t) if t == "int" || t.starts_with("int") || t.starts_with("uint") => 1,
        Some(t) if t.starts_with("num") => 2,
        Some("str") => 3,
        _ => 0,
    }
}

impl Interpreter {
    /// The BUILDPLAN of `class` itself (its own attributes and submethods),
    /// in Rakudo's order: the init tasks (none when the class declares a
    /// `BUILD`), `BUILD`, the `is required` checks, the defaults, the no-op
    /// entries of attributes left alone, `TWEAK`.
    // Cost: O(a), a = the attributes `class` declares.
    pub(crate) fn derive_buildplan(&mut self, class: &str) -> Value {
        use task_code::*;
        let attrs: Vec<ClassAttributeDef> = self
            .registry()
            .classes
            .get(class)
            .map(|def| def.attributes.clone())
            .unwrap_or_default();
        let class_obj = Value::package(Symbol::intern(class));
        let build = self.own_phase_submethod(class, "BUILD");
        let tweak = self.own_phase_submethod(class, "TWEAK");
        let attr_name = |attr: &ClassAttributeDef| Value::str(format!("{}!{}", attr.sigil, attr.name));
        let task = |items: Vec<Value>| Value::vm_array(items);
        let mut plan = Vec::new();
        if build.is_none() {
            for attr in &attrs {
                if !self.is_attribute_buildable(class, &attr.name) {
                    continue;
                }
                let kind = native_kind(attr.type_constraint.as_deref());
                plan.push(task(vec![
                    Value::int(INIT_OBJ + kind),
                    class_obj.clone(),
                    attr_name(attr),
                    Value::str(attr.name.clone()),
                ]));
            }
        }
        plan.extend(build);
        for attr in &attrs {
            if let Some(why) = &attr.is_required {
                let why = match why {
                    Some(reason) => Value::str(reason.clone()),
                    None => Value::int(1),
                };
                plan.push(task(vec![
                    Value::int(REQUIRED),
                    class_obj.clone(),
                    attr_name(attr),
                    why,
                ]));
            }
        }
        for attr in &attrs {
            if !Self::has_build_default(attr) {
                continue;
            }
            let kind = native_kind(attr.type_constraint.as_deref());
            let payload = self.attribute_default_payload(attr, class);
            plan.push(task(vec![
                Value::int(DEFAULT_OBJ + kind),
                class_obj.clone(),
                attr_name(attr),
                payload,
            ]));
        }
        for attr in &attrs {
            if Self::has_build_default(attr)
                || attr.is_required.is_some()
                || native_kind(attr.type_constraint.as_deref()) != 0
            {
                continue;
            }
            plan.push(task(vec![Value::int(NOOP), class_obj.clone(), attr_name(attr)]));
        }
        plan.extend(tweak);
        Value::vm_array(plan)
    }

    /// The BUILDALLPLAN of `class`: the BUILDPLANs of its MRO, least derived
    /// first, without the no-op entries. A plan the MOP stored for a class
    /// (its own, or one a module bound) is used as stored.
    // Cost: O(m * a), m = the MRO's length, a = attributes per class.
    pub(crate) fn derive_buildallplan(&mut self, class: &str) -> Value {
        let mro = self.class_mro(class);
        let mut all = Vec::new();
        for layer in mro.iter().rev().map(|s| s.as_str()) {
            if layer == "Any" || layer == "Mu" || !self.registry().classes.contains_key(layer) {
                continue;
            }
            let plan = self.buildplan_of(layer);
            let ValueView::Array(items, _) = plan.view() else {
                continue;
            };
            all.extend(items.iter().filter(|task| !Self::is_noop_task(task)).cloned());
        }
        Value::vm_array(all)
    }

    /// `class`'s BUILDPLAN: the stored one, else a freshly derived one.
    // Cost: O(1) when stored, else `derive_buildplan`'s.
    pub(crate) fn buildplan_of(&mut self, class: &str) -> Value {
        let stored = self
            .registry()
            .buildplans
            .get(class)
            .and_then(|state| state.plan.clone());
        stored.unwrap_or_else(|| self.derive_buildplan(class))
    }

    /// `class`'s BUILDALLPLAN: the stored one, else a freshly derived one.
    // Cost: O(1) when stored, else `derive_buildallplan`'s.
    pub(crate) fn buildallplan_of(&mut self, class: &str) -> Value {
        let stored = self
            .registry()
            .buildplans
            .get(class)
            .and_then(|state| state.all_plan.clone());
        stored.unwrap_or_else(|| self.derive_buildallplan(class))
    }

    /// Whether `attr` has a written initializer, which its default task runs
    /// (not a type-object seed, nor an `is default(...)` value alone).
    // Cost: O(1).
    fn has_build_default(attr: &ClassAttributeDef) -> bool {
        attr.default.is_some() && !attr.default_is_seed && !attr.default_is_trait
    }

    /// Whether `task` is a `[1000, type, name]` no-op entry.
    // Cost: O(1).
    fn is_noop_task(task: &Value) -> bool {
        match task.view() {
            ValueView::Array(items, _) => items
                .first()
                .is_some_and(|code| code.as_int() == Some(task_code::NOOP)),
            _ => false,
        }
    }

    /// The `BUILD`/`TWEAK` submethod `class` declares itself, as a `Code`
    /// task (`Submethod`), or `None`.
    // Cost: O(1) plus one method lookup.
    fn own_phase_submethod(&mut self, class: &str, name: &str) -> Option<Value> {
        let declared = self
            .registry()
            .user_method_overloads(class, name)
            .is_some_and(|defs| defs.iter().any(|def| def.role_origin.is_none()));
        if !declared {
            return None;
        }
        self.classhow_find_method(&Value::package(Symbol::intern(class)), name)
    }

    /// A default task's payload: the default's value when it is a constant,
    /// else the `Code` that `Attribute.build` gives for it.
    // Cost: O(1) plus building the attribute's meta-object.
    fn attribute_default_payload(&self, attr: &ClassAttributeDef, owner: &str) -> Value {
        if let Some(value) = attr.default.as_ref().and_then(|d| d.literal()) {
            return value.clone();
        }
        let object = self.make_attribute_object(attr, owner);
        Self::nqp_attr_value(&object, "$!build").unwrap_or(Value::NIL)
    }

    /// `Metamodel::ClassHOW.create_BUILDPLAN(type)`: derive and store the
    /// type's BUILDPLAN and BUILDALLPLAN, which a HOW mixin overriding it then
    /// reads (and may replace) after its `callsame`.
    // Cost: O(m * a), see `derive_buildallplan`.
    pub(crate) fn mop_create_buildplan(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let Some(class) = args.first().map(|t| self.mop_receiver_owner(t)) else {
            return Ok(Value::NIL);
        };
        let plan = self.derive_buildplan(&class);
        self.registry_mut()
            .buildplans
            .entry(class.clone())
            .or_default()
            .plan = Some(plan);
        let all_plan = self.derive_buildallplan(&class);
        self.registry_mut()
            .buildplans
            .entry(class)
            .or_default()
            .all_plan = Some(all_plan);
        Ok(Value::NIL)
    }

    /// Call the `create_BUILDPLAN` and `publish_method_cache` overrides of
    /// `class`'s HOW, in the order Rakudo's `ClassHOW.compose` calls them.
    /// Only a HOW mixin can override them; any other HOW keeps the native
    /// path, which needs no call (ADR-14162 §3).
    // Cost: O(1) without a mixin HOW; else the overrides' own cost.
    pub(crate) fn run_how_compose_overrides(&mut self, class: &str) -> Result<(), RuntimeError> {
        let Some(how) = self.registry().class_how_values.get(class).cloned() else {
            return Ok(());
        };
        if !matches!(how.view(), ValueView::Mixin(..)) {
            return Ok(());
        }
        let type_obj = Value::package(Symbol::intern(class));
        for hook in ["create_BUILDPLAN", "publish_method_cache"] {
            if self.mixin_has_hook(&how, hook) {
                self.call_method_with_values(how.clone(), hook, vec![type_obj.clone()])?;
            }
        }
        Ok(())
    }

    /// `Metamodel::ClassHOW.BUILDPLAN(type)` / `.BUILDALLPLAN(type)`: the
    /// plan as a high-level `List` of tasks, a list task itself a `List`, as
    /// Rakudo's `nqp::hllize`d answer reads.
    // Cost: O(t) tasks on top of `buildplan_of` / `buildallplan_of`.
    pub(crate) fn mop_buildplan(&mut self, args: Vec<Value>, all: bool) -> Result<Value, RuntimeError> {
        let Some(class) = args.first().map(|t| self.mop_receiver_owner(t)) else {
            return Ok(Value::NIL);
        };
        let plan = if all {
            self.buildallplan_of(&class)
        } else {
            self.buildplan_of(&class)
        };
        let tasks: Vec<Value> = match plan.view() {
            ValueView::Array(items, _) => items
                .iter()
                .map(|task| task.clone().hllize_vm_collection())
                .collect(),
            _ => Vec::new(),
        };
        Ok(Value::array(tasks))
    }
}
