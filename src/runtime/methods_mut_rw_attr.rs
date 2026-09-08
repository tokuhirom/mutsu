use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Whether a statement is (or returns) an explicit `return-rw ...` call.
    pub(crate) fn stmt_contains_return_rw_call(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(expr) | Stmt::Return(expr) => {
                matches!(expr, Expr::Call { name, .. } if name == "return-rw")
            }
            _ => false,
        }
    }

    /// The attribute an `is rw` method exposes as its assignable lvalue: the
    /// bare `$!attr`/`@!attr`/`%!attr` (or `return-rw` of one) its body
    /// evaluates to. Returns the attribute name and its sigil.
    ///
    /// An `is rw` routine returns its *last* expression's container, so the
    /// last statement is what decides — the earlier ones are ordinary code that
    /// still has to run (HTTP::Request's
    /// `multi method scheme(--> Str:D) is rw { without $!scheme { … }; $!scheme }`
    /// lazily defaults the attribute before exposing it). Every caller invokes
    /// the method anyway to read its current value, so those side effects
    /// happen. An explicit `return-rw $!attr` anywhere in the body also counts:
    /// it decides the return value wherever it sits.
    pub(crate) fn rw_method_attribute_target(body: &[Stmt]) -> Option<(String, char)> {
        let significant = || body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
        let last = significant().next_back()?;
        let extract_attr = |expr: &Expr| -> Option<(String, char)> {
            match expr {
                Expr::Var(name) if name.starts_with('!') && name.len() > 1 => {
                    Some((name[1..].to_string(), '$'))
                }
                Expr::ArrayVar(name) if name.starts_with('!') && name.len() > 1 => {
                    Some((name[1..].to_string(), '@'))
                }
                Expr::HashVar(name) if name.starts_with('!') && name.len() > 1 => {
                    Some((name[1..].to_string(), '%'))
                }
                Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => {
                    match &args[0] {
                        Expr::Var(attr) if attr.starts_with('!') && attr.len() > 1 => {
                            Some((attr[1..].to_string(), '$'))
                        }
                        Expr::ArrayVar(attr) if attr.starts_with('!') && attr.len() > 1 => {
                            Some((attr[1..].to_string(), '@'))
                        }
                        Expr::HashVar(attr) if attr.starts_with('!') && attr.len() > 1 => {
                            Some((attr[1..].to_string(), '%'))
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        };
        let attr_of = |stmt: &Stmt| match stmt {
            Stmt::Expr(expr) | Stmt::Return(expr) => extract_attr(expr),
            _ => None,
        };
        attr_of(last).or_else(|| {
            significant()
                .find(|s| Self::stmt_contains_return_rw_call(s))
                .and_then(attr_of)
        })
    }

    /// The type check every attribute store applies to the value on its way in.
    ///
    /// One rule, two callers: the generated public accessor
    /// (`$obj.attr = v`) and the hand-written `is rw` method that exposes the
    /// same attribute (`method acc is rw { $!attr }`; `$obj.acc = v`). An
    /// `is rw` method over a bare `$!attr` *is* an accessor, so the two must
    /// agree — the method store used to skip the check entirely, letting
    /// `$obj.acc = "str"` land a `Str` in a `has Int $.n`.
    ///
    /// `Nil` is exempt: it never reaches the attribute, because
    /// [`Self::attr_store_nil_default`] replaces it with the declared default
    /// or the declared type object first.
    pub(crate) fn check_attr_store_type(
        &mut self,
        class_name: &str,
        attr: &str,
        attr_sigil: char,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        // For `@`/`%` attributes the constraint applies to the elements, not to
        // the container being stored; those checks live at the call sites.
        if attr_sigil != '$' || value.is_nil() {
            return Ok(());
        }
        let Some(type_constraint) = self.get_attr_type_constraint(class_name, attr) else {
            return Ok(());
        };
        if self.type_matches_value(&type_constraint, value)
            || self.is_container_subclass(&type_constraint)
        {
            return Ok(());
        }
        Err(RuntimeError::typecheck_assignment(
            &type_constraint,
            value,
            Some(&format!("$!{}", attr)),
        ))
    }

    /// What assigning `Nil` to attribute `attr` actually stores.
    ///
    /// Raku's `=` restores a container's *default* when handed `Nil`: the
    /// `is default(...)` value when the declaration has one, otherwise the
    /// declared type object (`A` for `has A $.a`) — and `Any` for an untyped
    /// scalar attribute, which is what an untyped `Scalar` container defaults
    /// to. Anything but `Nil` passes straight through.
    ///
    /// Shared by the accessor store and the `is rw` method store for the same
    /// reason [`Self::check_attr_store_type`] is: they are two spellings of one
    /// accessor. The method store used to apply only the `is default(...)` half
    /// (so `$obj.acc = Nil` left a literal `Nil` in a typed attribute), and the
    /// accessor store only the typed half (so an *untyped* attribute kept `Nil`
    /// instead of resetting to `Any`).
    pub(crate) fn attr_store_nil_default(
        &mut self,
        class_name: &str,
        attr: &str,
        attr_sigil: char,
        value: Value,
    ) -> Value {
        if !value.is_nil() {
            return value;
        }
        if let Some(def) = self.class_attribute_default_with_role_fallback(class_name, attr) {
            return def;
        }
        // An `@`/`%` attribute's Nil decay is the container's own business
        // (`decay_nil_container_elements`), not a type-object reset.
        if attr_sigil != '$' {
            return value;
        }
        let type_name = self
            .get_attr_type_constraint(class_name, attr)
            .unwrap_or_else(|| "Any".to_string());
        Value::package(Symbol::intern(&type_name))
    }

    /// Detect an `is rw` method whose body returns an *indexed* attribute
    /// element — `@!attr[$param]` or `%!attr{$param}` — where the index/key is
    /// a single positional parameter. Returns `(attr_name, param_name,
    /// is_positional)`. Such a method exposes the element as a writable lvalue,
    /// so `$obj.at($i) = v` assigns into the attribute container's element.
    /// (The simpler `{ $!attr }` form is handled by
    /// `rw_method_attribute_target`.)
    pub(crate) fn rw_method_indexed_attr_target(body: &[Stmt]) -> Option<(String, String, bool)> {
        let first = body.iter().find(|s| !matches!(s, Stmt::SetLine(_)))?;
        let expr = match first {
            Stmt::Expr(e) | Stmt::Return(e) => e,
            _ => return None,
        };
        // Unwrap `return-rw EXPR`.
        let expr = match expr {
            Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => &args[0],
            other => other,
        };
        if let Expr::Index {
            target,
            index,
            is_positional,
        } = expr
            && let Expr::Var(param) = index.as_ref()
        {
            // `@!attr[...]` parses as `ArrayVar("!attr")`, `%!attr{...}` as
            // `HashVar("!attr")`.
            let attr = match target.as_ref() {
                Expr::ArrayVar(a) | Expr::HashVar(a) if a.starts_with('!') && a.len() > 1 => {
                    &a[1..]
                }
                _ => return None,
            };
            return Some((attr.to_string(), param.clone(), *is_positional));
        }
        None
    }

    /// Detect an `is rw` method whose body returns an indexed element of the
    /// invocant itself — `self[EXPR]` (or `self{EXPR}`) — as happens in an
    /// `is Array` subclass accessor (`method z() is rw { self[2] }`). Returns
    /// `(index_expr, is_positional)`; the element lives in the instance's
    /// backing `__mutsu_array_storage`, so `$obj.z = v` writes `storage[2]`.
    pub(crate) fn rw_method_self_index_target(body: &[Stmt]) -> Option<(Expr, bool)> {
        let first = body.iter().find(|s| !matches!(s, Stmt::SetLine(_)))?;
        let expr = match first {
            Stmt::Expr(e) | Stmt::Return(e) => e,
            _ => return None,
        };
        let expr = match expr {
            Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => &args[0],
            other => other,
        };
        if let Expr::Index {
            target,
            index,
            is_positional,
        } = expr
            && matches!(target.as_ref(), Expr::BareWord(w) | Expr::Var(w) if w == "self")
        {
            return Some(((**index).clone(), *is_positional));
        }
        None
    }

    /// Store `new_value` into the container the attribute `existing` already
    /// holds, keeping that container's identity. Returns `false` when the two
    /// are not the same kind of container, in which case the caller falls back
    /// to rebinding the attribute.
    ///
    /// `method items { @!items }` hands back the attribute's own Array -- in
    /// raku it *is* that object -- so `$obj.items = LIST` and
    /// `$obj.items[$i] = v` store INTO that container; they do not rebind the
    /// attribute to a fresh one. mutsu rebuilt the whole attribute map around a
    /// new node instead, which was wrong twice over:
    ///
    /// - every alias went stale: `my @a := $obj.items; $obj.items = (1,2,3)`
    ///   left `@a` empty where raku shows `[1 2 3]`;
    /// - the container node moved on every write, so the ADR-0068 element-store
    ///   guard -- which keys on the container node when the container is not
    ///   celled -- locked a different stripe each time and excluded nothing.
    ///   Twenty threads writing 1000 distinct indices through such an accessor
    ///   landed 304-543 of them (rakudo: 1000), and the whole-map commit in
    ///   `write_back_sharing` clobbered concurrent writes to *other* attributes
    ///   as well. Storing in place removes both the stale map copy and the
    ///   moving key.
    ///
    /// The auto-generated accessor for the same attribute already behaved this
    /// way, which is why `has @.seen` was measured clean at 0/240 while
    /// `method seen { @!seen }` lost half its writes.
    pub(crate) fn store_into_attr_container(existing: &Value, new_value: &Value) -> bool {
        match (existing.view(), new_value.view()) {
            (ValueView::Array(dst, _), ValueView::Array(src, _)) => {
                if crate::gc::Gc::ptr_eq(&dst, &src) {
                    return true;
                }
                let _guard =
                    crate::value::container_lock::ContainerStructGuard::acquire_for(None, existing);
                unsafe { crate::value::gc_contents_mut(&dst) }.adopt_state_from(&src);
                true
            }
            (ValueView::Hash(dst), ValueView::Hash(src)) => {
                if crate::gc::Gc::ptr_eq(&dst, &src) {
                    return true;
                }
                let _guard =
                    crate::value::container_lock::ContainerStructGuard::acquire_for(None, existing);
                unsafe { crate::value::gc_contents_mut(&dst) }.adopt_state_from(&src);
                true
            }
            _ => false,
        }
    }

    /// Assign `value` into element `index_value` of the array/hash attribute
    /// `attr_name` on the instance, then write the updated instance back through
    /// `target_var`. Backs `$obj.rw-method(idx) = value` where the method
    /// returns `@!attr[idx]` / `%!attr{key}`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn assign_rw_indexed_attr(
        &mut self,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        class_name: Symbol,
        target_id: u64,
        target_var: Option<&str>,
        attr_name: &str,
        index_value: Value,
        is_positional: bool,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let mut updated = attributes.to_map();
        let container = updated.get(attr_name).cloned().unwrap_or(Value::NIL);
        let new_container = if is_positional {
            let idx = crate::runtime::utils::to_int(&index_value);
            let (mut data, kind) = match container.view() {
                ValueView::Array(items, kind) => ((**items).clone(), kind),
                ValueView::Nil => (
                    crate::value::ArrayData::new(Vec::new()),
                    crate::value::ArrayKind::Array,
                ),
                _ => {
                    return Err(RuntimeError::assignment_ro_typename(
                        &crate::value::what_type_name(&container),
                        &value.to_string_value(),
                    ));
                }
            };
            if idx < 0 {
                return Err(RuntimeError::new(format!(
                    "Index {} out of range for rw element assignment",
                    idx
                )));
            }
            let idx = idx as usize;
            if idx >= data.items().len() {
                data.items_mut().resize(idx + 1, Value::NIL);
            }
            data.items_mut()[idx] = value.clone();
            Value::array_with_kind(crate::gc::Gc::new(data), kind)
        } else {
            let key = index_value.to_string_value();
            let mut data = match container.view() {
                ValueView::Hash(items) => (**items).clone(),
                ValueView::Nil => crate::value::HashData::new(std::collections::HashMap::new()),
                _ => {
                    return Err(RuntimeError::assignment_ro_typename(
                        &crate::value::what_type_name(&container),
                        &value.to_string_value(),
                    ));
                }
            };
            data.map.insert(key, value.clone());
            Value::hash_with_data(crate::gc::Gc::new(data))
        };
        updated.insert(attr_name.to_string(), new_container);
        if let Some(var_name) = target_var {
            self.env.insert(
                var_name.to_string(),
                Value::write_back_sharing(attributes, class_name, updated, target_id),
            );
        }
        Ok(value)
    }
}
