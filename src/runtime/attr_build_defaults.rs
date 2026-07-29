//! `has $.x = <default>` initializers relative to `submethod BUILD`.
//!
//! Raku runs a user `BUILD` **first** and only then applies each attribute
//! initializer, and only to attributes `BUILD` did not set. Two things follow,
//! both observable: an initializer that reads a sibling attribute sees the
//! value `BUILD` gave it, and an initializer with a side effect does not run at
//! all when `BUILD` set the attribute.
//!
//! mutsu implements that with a *seed-and-defer* pass. A class with a BUILD
//! phase seeds every not-provided attribute that has an initializer with the
//! same value an initializer-less attribute would get (its nominal type object
//! / native zero / empty container), records the initializer as deferred, runs
//! BUILD against the real instance, and only then applies each deferred
//! initializer — skipping the ones BUILD touched.
//!
//! "BUILD touched it" is two conditions, because rakudo's marker (a null
//! attribute slot) has no equivalent here:
//!
//! 1. an assignment reached the instance's shared cell — recorded by
//!    [`crate::runtime::Interpreter::record_build_attr_write`] into the frame
//!    pushed here. This is what makes an explicit `$!x = Any` (and a
//!    `BUILD(:$!x)` attributive parameter that was never passed) suppress the
//!    initializer, exactly like rakudo;
//! 2. the slot no longer holds its seed — which catches an in-place container
//!    mutation such as `@!xs.push(9)`, whose vivification rakudo also treats as
//!    "set" (`has @.xs = 1,2,3` + that push yields `[9]`, not `[1,2,3,9]`).

use std::collections::HashSet;

use super::{Interpreter, RuntimeError};
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::{AttrMap, Value, ValueView};

/// An attribute whose initializer was postponed until after the BUILD phase.
pub(crate) struct DeferredAttrDefault {
    pub(crate) name: String,
    pub(crate) sigil: char,
    /// The `has $.x = <expr>` initializer, when the attribute has one.
    pub(crate) default: Option<Expr>,
    /// An `is built(&code)` override, which takes precedence over `default`.
    pub(crate) build_override: Option<Value>,
    /// The value the slot was seeded with; the slot still holding it is half of
    /// the "BUILD left this alone" test (see the module docs).
    pub(crate) seed: Value,
}

impl Interpreter {
    /// Start recording attribute writes against `inv`'s shared cell for the
    /// duration of its BUILD phase. Paired with [`Self::pop_build_write_frame`].
    pub(crate) fn push_build_write_frame(&self, inv: &Value) {
        let Some(cell) = Self::self_instance_attrs(inv) else {
            return;
        };
        self.build_attr_writes
            .borrow_mut()
            .push(super::BuildWriteFrame {
                cell_addr: crate::gc::Gc::as_ptr(&cell) as usize,
                written: HashSet::new(),
            });
    }

    /// Stop recording and return the attribute cell keys BUILD assigned.
    pub(crate) fn pop_build_write_frame(&self) -> HashSet<Symbol> {
        self.build_attr_writes
            .borrow_mut()
            .pop()
            .map(|f| f.written)
            .unwrap_or_default()
    }

    /// Whether the slot still holds exactly what the seed pass put there, i.e.
    /// nothing in BUILD changed it. Containers compare by emptiness rather than
    /// identity: `@!xs.push(9)` mutates the seeded array in place, so identity
    /// would still match while the attribute is plainly no longer untouched.
    fn attr_slot_still_seed(cur: &Value, seed: &Value) -> bool {
        match (cur.view(), seed.view()) {
            (ValueView::Package(a), ValueView::Package(b)) => a == b,
            (ValueView::Array(items, _), ValueView::Array(..)) => items.is_empty(),
            (ValueView::Hash(map), ValueView::Hash(_)) => map.is_empty(),
            (ValueView::Nil, ValueView::Nil) => true,
            (ValueView::Int(a), ValueView::Int(b)) => a == b,
            (ValueView::Num(a), ValueView::Num(b)) => a == b,
            (ValueView::Str(a), ValueView::Str(b)) => *a == *b,
            _ => false,
        }
    }

    /// Apply the deferred initializers to `inv` now that its BUILD phase has
    /// run, skipping every attribute BUILD set. `written` comes from
    /// [`Self::pop_build_write_frame`].
    pub(crate) fn apply_post_build_attr_defaults(
        &mut self,
        class_key: &str,
        class_name: Symbol,
        inv: &Value,
        deferred: &[DeferredAttrDefault],
        written: &HashSet<Symbol>,
    ) -> Result<(), RuntimeError> {
        let Some(cell) = Self::self_instance_attrs(inv) else {
            return Ok(());
        };
        let attr_type_constraints = self.collect_attribute_type_constraints(class_key);
        for d in deferred {
            let key = Symbol::intern(&d.name);
            if written.contains(&key) {
                continue;
            }
            let current = cell.as_map().get(key).cloned();
            match &current {
                Some(cur) if Self::attr_slot_still_seed(cur, &d.seed) => {}
                // The key vanished (a BUILD that replaced the whole map) or the
                // slot changed: BUILD owns the value now.
                _ => continue,
            }
            let val = if let Some(build_override) = &d.build_override {
                let val = self.call_sub_value(build_override.clone(), Vec::new(), false)?;
                Self::coerce_attr_value_by_sigil(val, d.sigil)
            } else if let Some(expr) = &d.default {
                // A literal needs no evaluation context at all — the same fast
                // path the pre-BUILD pass takes for parser-generated zeroes.
                if let Expr::Literal(lit_val) = expr {
                    Self::coerce_attr_value_by_sigil(lit_val.clone(), d.sigil)
                } else {
                    let attrs = cell.to_map();
                    let expr = expr.clone();
                    let val =
                        self.eval_attr_default_expr(class_key, class_name, &expr, inv, &attrs)?;
                    Self::coerce_attr_value_by_sigil(val, d.sigil)
                }
            } else {
                continue;
            };
            let val = if d.sigil == '$'
                && let Some(tc) = attr_type_constraints.get(&d.name)
                && crate::runtime::types::is_coercion_constraint(tc)
            {
                self.coerce_value_for_constraint(tc, val)
            } else {
                val
            };
            // Raku: assigning `Nil` resets a container to its declared type's
            // default — the nominal type object (`Any` when untyped). An
            // attribute initializer is an assignment too, so `has Str $.n = Nil`
            // reads back as `Str`, not `Nil` (and a `:D` violation then reports
            // `got Str`, the way rakudo does).
            let val = if val.is_nil() {
                self.seed_attr_value(class_key, &d.name, d.sigil, &attr_type_constraints)
            } else {
                val
            };
            cell.insert(key, val);
        }
        Ok(())
    }

    /// Build the initial container for an `@`/`%` attribute declared with an
    /// `is Type` container trait (`has @.a is Buf`, `has %.h is TypeConverter`).
    /// A parameterized `Array[T]` is built directly with element-type metadata
    /// (a `Package` built from the string name would lose its type parameter on
    /// `.new`); any other type is produced by dispatching to its `.new`, falling
    /// back to a plain empty container when that fails.
    pub(crate) fn build_is_type_container(&mut self, type_name: &str, sigil: char) -> Value {
        if let Some(inner) = type_name
            .strip_prefix("Array[")
            .or_else(|| type_name.strip_prefix("array["))
            .and_then(|s| s.strip_suffix(']'))
        {
            let arr = Value::real_array(Vec::new());
            return self.tag_container_metadata(
                arr,
                super::ContainerTypeInfo {
                    value_type: inner.trim().to_string(),
                    key_type: None,
                    declared_type: Some(type_name.to_string()),
                },
            );
        }
        let type_obj = Value::package(Symbol::intern(type_name));
        match self.call_method_with_values(type_obj, "new", vec![]) {
            Ok(v) => v,
            Err(_) if sigil == '@' => Value::real_array(Vec::new()),
            Err(_) => Value::hash(std::collections::HashMap::new()),
        }
    }

    /// The value an attribute with no initializer starts life with: an empty
    /// container for `@`/`%` (carrying the declared element type / `is Type`
    /// container), a native zero for a native-typed `$`, and otherwise the
    /// attribute's nominal type object (`has $!z` reads as `Any`, `has Int $!x`
    /// as `Int` — not `Nil`, matching raku).
    ///
    /// This is also the *seed* an attribute that DOES have an initializer gets
    /// before BUILD runs, which is why it lives on its own: BUILD observes the
    /// same pre-initializer state rakudo shows it.
    pub(crate) fn seed_attr_value(
        &mut self,
        class_key: &str,
        attr_name: &str,
        sigil: char,
        attr_type_constraints: &std::collections::HashMap<String, String>,
    ) -> Value {
        match sigil {
            '@' => {
                // Check for `is Type` trait (e.g. `has @.a is Buf`)
                let is_type = self
                    .registry()
                    .class_attribute_is_types
                    .get(&(class_key.to_string(), attr_name.to_string()))
                    .cloned();
                if let Some(type_name) = is_type {
                    self.build_is_type_container(&type_name, '@')
                } else {
                    let arr = Value::real_array(Vec::new());
                    // Register element type constraint for typed array attributes
                    let tc = self
                        .registry()
                        .classes
                        .get(class_key)
                        .and_then(|cd| cd.attribute_types.get(attr_name))
                        .cloned();
                    match tc {
                        Some(tc) => self.tag_container_metadata(
                            arr,
                            super::ContainerTypeInfo {
                                value_type: tc,
                                key_type: None,
                                declared_type: None,
                            },
                        ),
                        None => arr,
                    }
                }
            }
            '%' => {
                // Check for `is Type` trait (e.g. `has %.h is BagHash`)
                let is_type = self
                    .registry()
                    .class_attribute_is_types
                    .get(&(class_key.to_string(), attr_name.to_string()))
                    .cloned();
                if let Some(type_name) = is_type {
                    self.build_is_type_container(&type_name, '%')
                } else {
                    let h = Value::hash(std::collections::HashMap::new());
                    // Register value type constraint for typed hash attributes
                    let tc = self
                        .registry()
                        .classes
                        .get(class_key)
                        .and_then(|cd| cd.attribute_types.get(attr_name))
                        .cloned();
                    match tc {
                        Some(tc) => self.tag_container_metadata(
                            h,
                            super::ContainerTypeInfo {
                                value_type: tc,
                                key_type: None,
                                declared_type: None,
                            },
                        ),
                        None => h,
                    }
                }
            }
            _ => match attr_type_constraints.get(attr_name).map(String::as_str) {
                Some(
                    "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                    | "uint32" | "uint64" | "byte" | "atomicint",
                ) => Value::int(0),
                Some("num" | "num32" | "num64") => Value::num(0.0),
                Some("str") => Value::str("".to_string()),
                Some(tc) => {
                    let nominal = self.nominal_type_object_name_for_constraint(tc);
                    Value::package(Symbol::intern(&nominal))
                }
                None => Value::package(Symbol::intern("Any")),
            },
        }
    }

    /// Evaluate one `has $.x = <expr>` initializer with `self_val` bound as
    /// `self` and every already-initialized attribute reachable as `$!a` / `$.a`
    /// (so `has $.c = $!a + $!b` and `has $.total = self.a + self.b` work), in
    /// the class's own package so class-scoped subs resolve. Shared by the
    /// pre-BUILD pass and the post-BUILD pass above, which differ only in what
    /// `self_val` is: a snapshot instance before BUILD, the real instance after.
    pub(crate) fn eval_attr_default_expr(
        &mut self,
        class_key: &str,
        class_name: Symbol,
        expr: &Expr,
        self_val: &Value,
        attrs: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let old_self = self.env.get("self").cloned();
        self.env.insert("self".to_string(), self_val.clone());
        // `::?CLASS` in a default (e.g. `has $.Version = ::?CLASS.^ver` composed
        // from a role) resolves through `?CLASS`; bind it to the class being built.
        let old_class = self.env.get("?CLASS").cloned();
        self.env
            .insert("?CLASS".to_string(), Value::package(class_name));
        let mut saved_attr_env: Vec<(String, Option<Value>)> = Vec::new();
        for (a_name, a_val) in attrs {
            let bang = format!("!{}", a_name);
            let dot = format!(".{}", a_name);
            saved_attr_env.push((bang.clone(), self.env.get(&bang).cloned()));
            saved_attr_env.push((dot.clone(), self.env.get(&dot).cloned()));
            self.env.insert(bang, a_val.clone());
            self.env.insert(dot, a_val.clone());
        }
        // Temporarily switch to the class package so that class-scoped subs
        // (e.g. `sub inner`) are found when evaluating the initializer.
        let saved_package = self.current_package();
        self.set_current_package(class_key.to_string());
        let result = self.eval_block_value(&[Stmt::Expr(expr.clone())]);
        self.set_current_package(saved_package);
        for (key, old_val) in saved_attr_env {
            if let Some(v) = old_val {
                self.env.insert(key, v);
            } else {
                self.env.remove(&key);
            }
        }
        if let Some(old) = old_self {
            self.env.insert("self".to_string(), old);
        } else {
            self.env.remove("self");
        }
        if let Some(old) = old_class {
            self.env.insert("?CLASS".to_string(), old);
        } else {
            self.env.remove("?CLASS");
        }
        result
    }
}
