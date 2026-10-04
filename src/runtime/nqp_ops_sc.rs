//! The serialization-context `nqp::` ops (#11504): `createsc`,
//! `scsetobj`, `scgetobjidx`, `setobjsc`, `getobjsc`, `pushcompsc`, ...
//!
//! In MoarVM a serialization context (an `SCRef`) is the table a compilation
//! unit's compile-time objects and code refs are registered in, so that
//! `nqp::serialize` can write them into the precompiled unit and
//! `nqp::deserialize` can rebuild them. mutsu does not precompile to a
//! serialized object graph, so those two ops have no meaning here (they are
//! listed under "Not applicable" in `docs/nqp-op-coverage.md`). The context
//! itself is still an ordinary run-time data structure, and these ops build
//! and query it exactly as MoarVM's do:
//!
//! * an SC is identified by its handle string: `createsc` with a handle that
//!   already names one answers that same SC (MoarVM's instance-wide SC
//!   registry), so the registry is shared by every thread;
//! * `scsetobj` / `scsetcode` fill the SC's object and code-ref slots,
//!   growing the root list with nulls as needed;
//! * an object's OWNING SC (`setobjsc` / `getobjsc`) is a separate fact --
//!   `scsetobj` does not set it (both measured against rakudo);
//! * the compiling-SC stack (`pushcompsc` / `popcompsc`) is per thread, as
//!   MoarVM's thread context holds it.
//!
//! The SC value itself is an `SCRef` instance that carries only its handle;
//! the mutable body lives in the registry, so every alias of the SC sees
//! every update.

use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard};

use crate::gc::{Gc, RootVisitor};
use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The class an SC value belongs to (rakudo: `nqp::createsc('x').^name`).
const SC_CLASS: &str = "SCRef";
/// The attribute of an `SCRef` instance holding its handle.
const HANDLE: &str = "handle";

/// One serialization context's contents.
struct ScBody {
    /// The `SCRef` value `createsc` handed out for this handle.
    sc: Value,
    /// `scsetdesc`'s descriptor; null until set.
    desc: Option<String>,
    /// The root objects, by index (`scsetobj`); a gap reads as null.
    objects: Vec<Value>,
    /// The code refs, by block index (`scsetcode`).
    codes: Vec<Value>,
}

/// An object's identity, as `nqp::eqaddr` sees it: the instance id or heap
/// address of an object with identity, otherwise its value (an unboxed
/// integer or num, which `eqaddr` also compares by value -- as MoarVM's cached
/// small integers are one object per value).
#[derive(Hash, PartialEq, Eq)]
enum ObjectKey {
    Instance(u64),
    Address(usize),
    Value(String),
}

// Cost: O(1) for an object with identity; O(n), n = size of the value's
// `.WHICH` text, otherwise.
fn object_key(value: &Value) -> ObjectKey {
    match value.view() {
        ValueView::Instance { id, .. } => ObjectKey::Instance(id),
        ValueView::Array(a, _) => ObjectKey::Address(Gc::as_ptr(&a).addr()),
        ValueView::Hash(h) => ObjectKey::Address(Gc::as_ptr(&h).addr()),
        ValueView::Sub(s) => ObjectKey::Address(Gc::as_ptr(&s).addr()),
        ValueView::LazyList(l) => ObjectKey::Address(Gc::as_ptr(&l).addr()),
        ValueView::Str(s) => ObjectKey::Address(Arc::as_ptr(&s).addr()),
        ValueView::Seq(s) => ObjectKey::Address(Arc::as_ptr(&s).addr()),
        _ => ObjectKey::Value(crate::value::which_key::value_which_key(value)),
    }
}

/// The process-wide part of the SC state: every SC by handle, and every
/// object's owning SC.
#[derive(Default)]
struct ScRegistry {
    contexts: HashMap<String, ScBody>,
    /// `setobjsc`: each object's owning SC, by object identity.
    owners: HashMap<ObjectKey, Owner>,
}

/// An object's owning SC.
struct Owner {
    /// The object itself, held so that its address cannot be reused by
    /// another object while the entry stands.
    object: Value,
    sc: Value,
    /// The owning SC's handle.
    handle: String,
    /// The object's root index in its owning SC: the last `scsetobj` that
    /// stored it there after `setobjsc`, which `scgetobjidx` answers without
    /// a scan (MoarVM caches the index in the object header the same way).
    index: Option<usize>,
}

/// The serialization-context state of one interpreter (a field of the
/// `types` subsystem, `TypeState::sc`).
pub(crate) struct ScState {
    shared: Arc<Mutex<ScRegistry>>,
    /// The compiling-SC stack (`pushcompsc` / `popcompsc`).
    compiling: Vec<Value>,
}

impl ScState {
    pub(crate) fn new() -> Self {
        Self {
            shared: Arc::default(),
            compiling: Vec::new(),
        }
    }

    /// A spawned thread's copy: the same SC registry, its own (empty)
    /// compiling-SC stack.
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self {
            shared: Arc::clone(&self.shared),
            compiling: Vec::new(),
        }
    }

    fn registry(&self) -> MutexGuard<'_, ScRegistry> {
        self.shared.lock().unwrap_or_else(|poison| poison.into_inner())
    }

    /// Every value the SC state holds, for the GC root walk.
    // Cost: O(s + o + c), s = SCs, o = objects and code refs they hold,
    // c = objects with an owning SC.
    pub(crate) fn visit_roots(&self, visitor: &mut dyn RootVisitor) {
        crate::gc::visit_slice(visitor, &self.compiling);
        let registry = self.registry();
        for body in registry.contexts.values() {
            visitor.visit_value(&body.sc);
            crate::gc::visit_slice(visitor, &body.objects);
            crate::gc::visit_slice(visitor, &body.codes);
        }
        for owner in registry.owners.values() {
            visitor.visit_value(&owner.object);
            visitor.visit_value(&owner.sc);
        }
    }
}

/// How many operands each op takes (MoarVM's ops have a fixed count).
// Cost: O(1).
fn operand_count(op: &str) -> Option<usize> {
    Some(match op {
        "popcompsc" => 0,
        "createsc" | "scgethandle" | "scgetdesc" | "scobjcount" | "getobjsc" | "pushcompsc" => 1,
        "scsetdesc" | "scgetobjidx" | "setobjsc" => 2,
        "scsetobj" | "scsetcode" => 3,
        _ => return None,
    })
}

/// An operand with any argument wrapper and container stripped: the `nqp::`
/// layer reads raw values.
fn operand(args: &[Value], i: usize) -> Value {
    crate::runtime::types::unwrap_varref_value(args.get(i).cloned().unwrap_or(Value::NIL))
        .deref_container()
}

/// The handle of an `SCRef` operand.
// Cost: O(h), h = chars of the handle (copied).
fn sc_handle(op: &str, sc: &Value) -> Result<String, RuntimeError> {
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = sc.view()
        && class_name.resolve() == SC_CLASS
        && let Some(handle) = attributes.as_map().get(HANDLE)
    {
        return Ok(handle.to_string_value());
    }
    Err(RuntimeError::new(format!(
        "Must provide an SCRef operand to {op}"
    )))
}

fn index_operand(op: &str, args: &[Value], i: usize) -> Result<usize, RuntimeError> {
    usize::try_from(crate::runtime::to_int(&operand(args, i)))
        .map_err(|_| RuntimeError::new(format!("Negative index passed to {op}")))
}

/// Store `value` at `index`, growing `slots` with nulls up to it.
// Cost: O(1) amortized for an index at most one past the end; O(k), k = slots
// added, otherwise.
fn store_at(slots: &mut Vec<Value>, index: usize, value: Value) {
    if index >= slots.len() {
        slots.resize(index + 1, Value::NIL);
    }
    slots[index] = value;
}

impl Interpreter {
    /// Try a serialization-context `nqp::` op. `None` means "not an op this
    /// table knows" -- the end of the dispatch chain.
    pub(crate) fn call_nqp_op_sc(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let want = operand_count(op)?;
        if args.len() != want {
            return Some(Err(RuntimeError::new(format!(
                "Arg count {} doesn't equal required operand count {want} for op '{op}'",
                args.len()
            ))));
        }
        Some(self.nqp_sc_op(op, args))
    }

    fn nqp_sc_op(&mut self, op: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        let sc_state = &mut self.types.sc;
        match op {
            // nqp::createsc($handle): the SC with that handle, created empty
            // if there is none yet.
            // Cost: O(h), h = chars of $handle.
            "createsc" => {
                let handle = operand(args, 0).to_string_value();
                let mut registry = sc_state.registry();
                let body = registry.contexts.entry(handle.clone()).or_insert_with(|| {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(HANDLE.to_string(), Value::str(handle));
                    ScBody {
                        sc: Value::make_instance_without_destroy(Symbol::intern(SC_CLASS), attrs),
                        desc: None,
                        objects: Vec::new(),
                        codes: Vec::new(),
                    }
                });
                Ok(body.sc.clone())
            }
            // nqp::scgethandle($sc): the handle `createsc` was given.
            // Cost: O(h), h = chars of the handle.
            "scgethandle" => sc_handle(op, &operand(args, 0)).map(Value::str),
            // nqp::scsetdesc($sc, $desc) / nqp::scgetdesc($sc): the SC's
            // descriptor string, null until set; the setter answers $desc.
            // Cost: O(h + d), h = chars of the handle, d = chars of the descriptor.
            "scsetdesc" => {
                let handle = sc_handle(op, &operand(args, 0))?;
                let desc = operand(args, 1).to_string_value();
                if let Some(body) = sc_state.registry().contexts.get_mut(&handle) {
                    body.desc = Some(desc.clone());
                }
                Ok(Value::str(desc))
            }
            // Cost: O(h + d), h = chars of the handle, d = chars of the descriptor.
            "scgetdesc" => {
                let handle = sc_handle(op, &operand(args, 0))?;
                let registry = sc_state.registry();
                Ok(match registry.contexts.get(&handle).and_then(|b| b.desc.as_ref()) {
                    Some(desc) => Value::str_from(desc),
                    None => Value::NIL,
                })
            }
            // nqp::scsetobj($sc, $idx, $obj): make $obj the SC's root object
            // $idx; answers $obj. Does not make $sc the object's owning SC,
            // but when it already is, $idx becomes the object's cached index.
            // Cost: O(h + w) amortized, h = chars of the handle, w = chars of
            // $obj's `.WHICH` for an unboxed value (0 otherwise); plus O(k),
            // k = null slots added, for an index past the end.
            "scsetobj" => {
                let handle = sc_handle(op, &operand(args, 0))?;
                let index = index_operand(op, args, 1)?;
                let value = operand(args, 2);
                let mut registry = sc_state.registry();
                if let Some(body) = registry.contexts.get_mut(&handle) {
                    store_at(&mut body.objects, index, value.clone());
                }
                if let Some(owner) = registry.owners.get_mut(&object_key(&value))
                    && owner.handle == handle
                {
                    owner.index = Some(index);
                }
                Ok(value)
            }
            // nqp::scsetcode($sc, $idx, $code): make $code the SC's code ref
            // $idx; answers $code.
            // Cost: O(h) amortized, h = chars of the handle; plus O(k), k =
            // null slots added, for an index past the end.
            "scsetcode" => {
                let handle = sc_handle(op, &operand(args, 0))?;
                let index = index_operand(op, args, 1)?;
                let value = operand(args, 2);
                if let Some(body) = sc_state.registry().contexts.get_mut(&handle) {
                    store_at(&mut body.codes, index, value.clone());
                }
                Ok(value)
            }
            // nqp::scobjcount($sc): the number of root-object slots.
            // Cost: O(h), h = chars of the handle.
            "scobjcount" => {
                let handle = sc_handle(op, &operand(args, 0))?;
                let registry = sc_state.registry();
                let count = registry.contexts.get(&handle).map_or(0, |b| b.objects.len());
                Ok(Value::int(i64::try_from(count).unwrap_or(i64::MAX)))
            }
            // nqp::scgetobjidx($sc, $obj): the object's cached index when $sc
            // owns it, else the first root index holding it (both as MoarVM).
            // Cost: O(h + w) for an owned object with a cached index, h =
            // chars of the handle, w = chars of $obj's `.WHICH` for an
            // unboxed value (0 otherwise); O(h + w + e), e = root objects of
            // $sc, for the scan.
            "scgetobjidx" => {
                let handle = sc_handle(op, &operand(args, 0))?;
                let target = operand(args, 1);
                let registry = sc_state.registry();
                let cached = registry
                    .owners
                    .get(&object_key(&target))
                    .filter(|owner| owner.handle == handle)
                    .and_then(|owner| owner.index);
                cached
                    .or_else(|| {
                        registry.contexts.get(&handle).and_then(|body| {
                            body.objects.iter().position(|object| {
                                crate::runtime::utils::values_same_object(object, &target)
                            })
                        })
                    })
                    .map(|i| Value::int(i64::try_from(i).unwrap_or(i64::MAX)))
                    .ok_or_else(|| {
                        RuntimeError::new("Object does not exist in serialization context")
                    })
            }
            // nqp::setobjsc($obj, $sc) / nqp::getobjsc($obj): an object's
            // owning SC (null when it has none); the setter answers $obj.
            // Cost: O(h + w), h = chars of the handle, w = chars of $obj's
            // `.WHICH` for an unboxed value (0 otherwise).
            "setobjsc" => {
                let object = operand(args, 0);
                let sc = operand(args, 1);
                let handle = sc_handle(op, &sc)?;
                sc_state.registry().owners.insert(
                    object_key(&object),
                    Owner {
                        object: object.clone(),
                        sc,
                        handle,
                        index: None,
                    },
                );
                Ok(object)
            }
            // Cost: O(1) for an object with identity; O(n), n = chars of its
            // `.WHICH`, for an unboxed value.
            "getobjsc" => {
                let object = operand(args, 0);
                let registry = sc_state.registry();
                Ok(registry
                    .owners
                    .get(&object_key(&object))
                    .map_or(Value::NIL, |owner| owner.sc.clone()))
            }
            // nqp::pushcompsc($sc) / nqp::popcompsc(): this thread's stack of
            // SCs being compiled into; push answers $sc, pop the SC it removes.
            // Cost: O(h), h = chars of the handle.
            "pushcompsc" => {
                let sc = operand(args, 0);
                sc_handle(op, &sc)?;
                sc_state.compiling.push(sc.clone());
                Ok(sc)
            }
            // Cost: O(1).
            "popcompsc" => sc_state
                .compiling
                .pop()
                .ok_or_else(|| RuntimeError::new("No current compiling SC")),
            _ => Err(RuntimeError::new(format!(
                "Unsupported nqp:: op: nqp::{op}"
            ))),
        }
    }
}
