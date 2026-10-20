//! Running a replaced BUILDPLAN (ADR-14162 §4).
//!
//! A class whose plan the meta-object protocol replaced (a bind of its HOW's
//! `@!BUILDPLAN`/`@!BUILDALLPLAN`, or of an ancestor's) is constructed by
//! running its BUILDALLPLAN task by task, as Rakudo's `BUILDALL` does. Every
//! other class keeps the native constructor, which this module never touches.
//!
//! The per-attribute work is the native constructor's own: the named-argument
//! store coerces and type-checks as `build_native_default_instance` does, and a
//! default the plan was derived with evaluates through
//! `eval_attr_default_expr` in the declaration's scope.

use super::buildplan::task_code;
use super::*;

/// One list task of a plan, decoded.
struct PlanTask {
    code: i64,
    owner: String,
    /// `$!name`, as the plan spells it.
    name: String,
    payload: Option<Value>,
}

impl PlanTask {
    // Cost: O(1).
    fn decode(task: &Value) -> Option<Self> {
        let ValueView::Array(items, _) = task.view() else {
            return None;
        };
        let code = items.first()?.as_int()?;
        let owner = match items.get(1)?.view() {
            ValueView::Package(sym) => sym.resolve(),
            _ => return None,
        };
        Some(Self {
            code,
            owner,
            name: items.get(2)?.to_string_value(),
            payload: items.get(3).cloned(),
        })
    }

    /// The attribute's sigil and bare name (`$!x` -> `('$', "x")`).
    // Cost: O(n), n = the name's length.
    fn sigil_and_bare(&self) -> (char, &str) {
        let sigil = self.name.chars().next().unwrap_or('$');
        let bare = self
            .name
            .get(1..)
            .unwrap_or("")
            .trim_start_matches(['!', '.']);
        (sigil, bare)
    }
}

impl Interpreter {
    /// Whether `class` constructs by a replaced BUILDPLAN: some class of its
    /// MRO had its plan bound through the MOP.
    // Cost: O(1) while no plan was ever bound; else O(m), m = the MRO's length.
    pub(crate) fn buildplan_replaced_in_mro(&mut self, class: Symbol) -> bool {
        if self.registry().buildplans.is_empty() {
            return false;
        }
        let mro = self.class_mro(class.as_str());
        let registry = self.registry();
        mro.iter().any(|layer| {
            registry
                .buildplans
                .get(layer.as_str())
                .is_some_and(|state| state.replaced)
        })
    }

    /// Whether a class of `class`'s MRO declares its own `new`.
    // Cost: O(m), m = the MRO's length.
    pub(crate) fn mro_declares_user_new(&mut self, class: Symbol) -> bool {
        let mro = self.class_mro(class.as_str());
        let registry = self.registry();
        mro.iter().any(|layer| {
            registry.user_method_overloads(layer.as_str(), "new").is_some()
                || registry.method_entry_proto(layer.as_str(), "new").is_some()
        })
    }

    /// Construct `class` by running its BUILDALLPLAN with the named arguments
    /// in `args`. `None` for a positional argument, which the default
    /// constructor rejects on its own path.
    // Cost: O(t) tasks, plus each task's own cost (a user `Code` task's call).
    pub(crate) fn construct_by_buildplan(
        &mut self,
        class: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.iter().any(|a| !a.is_string_pair_value()) {
            return None;
        }
        Some(self.run_buildallplan(class, args))
    }

    // Cost: see `construct_by_buildplan`.
    fn run_buildallplan(&mut self, class: Symbol, args: &[Value]) -> Result<Value, RuntimeError> {
        let instance = self.create_instance(class);
        let plan = self.buildallplan_of(class.as_str());
        let ValueView::Array(tasks, _) = plan.view() else {
            return Ok(instance);
        };
        let tasks: Vec<Value> = tasks.iter().cloned().collect();
        for task in tasks {
            match PlanTask::decode(&task) {
                Some(task) => self.run_plan_task(&instance, &task, args)?,
                // A `Code` task: a `BUILD`/`TWEAK` submethod or a block a
                // module put in the plan, called with the instance and the
                // named arguments.
                None => {
                    let mut call_args = Vec::with_capacity(args.len() + 1);
                    call_args.push(instance.clone());
                    call_args.extend(args.iter().cloned());
                    self.eval_call_on_value(task, call_args)?;
                }
            }
        }
        Ok(instance)
    }

    // Cost: O(1) plus the attribute store or default evaluation.
    fn run_plan_task(
        &mut self,
        instance: &Value,
        task: &PlanTask,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        use task_code::*;
        let owner_obj = Value::package(Symbol::intern(&task.owner));
        let inited = || Self::nqp_attr_inited(instance, Some(&owner_obj), &task.name);
        match task.code {
            INIT_OBJ..=INIT_STR => {
                let Some(arg_name) = task.payload.as_ref().map(|p| p.to_string_value()) else {
                    return Ok(());
                };
                let provided = args.iter().find_map(|a| match a.view() {
                    ValueView::Pair(key, value) if key.as_str() == arg_name => Some(value.clone()),
                    _ => None,
                });
                if let Some(value) = provided {
                    let value = self.plan_store_value(task, value)?;
                    self.plan_bind_attr(instance, task, value);
                }
            }
            REQUIRED => {
                if !inited() {
                    let why = task
                        .payload
                        .as_ref()
                        .filter(|p| matches!(p.view(), ValueView::Str(_)))
                        .map(|p| p.to_string_value());
                    return Err(RuntimeError::attribute_required(&task.name, why.as_deref()));
                }
            }
            DEFAULT_OBJ..=DEFAULT_STR => {
                if !inited() {
                    let value = self.plan_default_value(instance, task)?;
                    self.plan_bind_attr(instance, task, value);
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// A named argument's value as the attribute stores it: coerced by sigil
    /// and coercion type, type-checked, itemized (as the native constructor).
    // Cost: O(1) plus the coercion and type check.
    fn plan_store_value(&mut self, task: &PlanTask, value: Value) -> Result<Value, RuntimeError> {
        let (sigil, bare) = task.sigil_and_bare();
        if sigil != '$' {
            return Ok(Self::coerce_provided_attr_value_by_sigil(value, sigil));
        }
        let value = match self.plan_attr_def(task).and_then(|def| def.type_constraint) {
            Some(tc) if crate::runtime::types::is_coercion_constraint(&tc) => {
                self.coerce_value_for_constraint(&tc, value)
            }
            _ => value,
        };
        let value = self.check_attr_store_type(&task.owner, bare, sigil, value)?;
        Ok(Self::itemize_attr_store_value(sigil, value))
    }

    /// The value a default task gives its attribute. A default the plan was
    /// derived with evaluates natively in its declaration's scope; a `Code` a
    /// module put there is called as Rakudo calls it, with the instance and
    /// the attribute's type; any other payload is the value itself.
    // Cost: O(1) plus the default's own evaluation.
    fn plan_default_value(&mut self, instance: &Value, task: &PlanTask) -> Result<Value, RuntimeError> {
        let (sigil, _) = task.sigil_and_bare();
        let payload = task.payload.clone().unwrap_or(Value::NIL);
        let native_thunk = matches!(payload.view(),
            ValueView::Sub(data) if data.name.as_str() == "<attribute-build>");
        let value = if native_thunk {
            let def = self.plan_attr_def(task);
            match def.as_ref().and_then(|d| d.default.clone()) {
                Some(arg) => {
                    let def = def.as_ref().expect("attribute definition");
                    let attrs = match instance.view() {
                        ValueView::Instance { attributes, .. } => attributes.as_map().clone(),
                        _ => AttrMap::default(),
                    };
                    let value = self.eval_attr_default_expr(
                        &task.owner,
                        Symbol::intern(&task.owner),
                        &arg,
                        instance,
                        &attrs,
                        super::attr_build_defaults::AttrDeclScope::of(def),
                    )?;
                    self.reify_list_attr_value(&value, sigil)?;
                    value
                }
                None => Value::NIL,
            }
        } else if self.type_matches_value("Code", &payload) {
            let type_obj = self
                .plan_attr_def(task)
                .and_then(|d| d.type_constraint)
                .map(|t| Value::package(Symbol::intern(&t)))
                .unwrap_or_else(|| Value::package(Symbol::intern("Any")));
            self.eval_call_on_value(payload, vec![instance.clone(), type_obj])?
        } else {
            payload
        };
        let value = Self::coerce_attr_value_by_sigil(value, sigil);
        Ok(Self::itemize_attr_store_value(sigil, value))
    }

    /// The registered declaration of the task's attribute.
    // Cost: O(a), a = the owner's attributes.
    fn plan_attr_def(&self, task: &PlanTask) -> Option<ClassAttributeDef> {
        let (sigil, bare) = task.sigil_and_bare();
        self.registry()
            .classes
            .get(&task.owner)?
            .attributes
            .iter()
            .find(|a| a.name == bare && a.sigil == sigil)
            .cloned()
    }

    /// Store `value` into the task's attribute slot of `instance`.
    // Cost: O(1).
    fn plan_bind_attr(&self, instance: &Value, task: &PlanTask, value: Value) {
        let (sigil, bare) = task.sigil_and_bare();
        let ValueView::Instance { attributes, .. } = instance.view() else {
            return;
        };
        let key = match self.registry().classes.get(&task.owner) {
            Some(def) => attribute_storage_key(&def.attributes, bare, sigil),
            None => Symbol::intern(bare),
        };
        attributes.insert(key, value);
    }
}
