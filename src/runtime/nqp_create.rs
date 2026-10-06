//! `nqp::create` and `.CREATE`: allocate an instance with no constructor run
//! (ADR-0121 D1/D3).
//!
//! Which allocation a type gets -- an empty codepoint store, a bare VM
//! storage class's array or hash, `new` for the built-in containers, or an
//! instance with every declared slot seeded -- is a pure function of the
//! type's name and the registry. So is the seeded slot template and whether
//! the class also needs an associative backing store. `nqp::create` re-derived
//! all of it on every call: a short-name derivation, two string-keyed
//! registry set probes, an MRO walk resolving each ancestor's name, and a
//! constructor-plan lookup, ~2,000 instructions of each call where MoarVM's
//! `create` is a REPR allocation. [`CreateMemo`] remembers both answers per
//! class for one registry write generation, which every registry mutation
//! bumps (`Interpreter::registry_mut` is the only write path), so no answer
//! can outlive a declaration.

use super::*;

/// How `nqp::create` allocates an instance of one type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CreateKind {
    /// `Uni` or a normal form: an empty codepoint store of that form, which
    /// nqp code fills with `nqp::push_i` / `nqp::strtocodes`. `CREATE` would
    /// hand back a bare type object, since a Uni's content is not a Raku
    /// attribute.
    Uni,
    /// A bare `is repr('VMHash')` class: mutsu's own hash, which IS the store.
    VmHash,
    /// A bare `is repr('VMArray')` class: mutsu's own array.
    VmArray,
    /// An `is repr('CArray')` class: an instance with native element storage
    /// of the type's `.^array_type` (#11209).
    CArray,
    /// A native array, a Buf/Blob, or a Map/Hash/List/Array: `.new` with no
    /// arguments. In mutsu these are indistinguishable from their own
    /// storage, and nqp code creates one to fill it with `nqp::bindkey` /
    /// `nqp::push` or to install a separately built store into it, which an
    /// attribute-less `Mu.CREATE` instance would not allow.
    New,
    /// Everything else: the REPR-level `CREATE` (never a user `CREATE`
    /// method -- rakudo runs none).
    Create,
}

impl CreateKind {
    /// The kind for a type named `name`, whose last `::` part is `short`.
    /// `vm_hash` / `vm_array` answer whether a bare VM storage class is
    /// registered under `short` (the sets are keyed by short name, see
    /// `register_vm_storage_class`).
    // Cost: O(n), n = chars of the name.
    fn of(name: &str, vm_hash: bool, vm_array: bool) -> Self {
        if matches!(name, "Uni" | "NFC" | "NFD" | "NFKC" | "NFKD") {
            Self::Uni
        } else if vm_hash || name == "Rakudo::Internals::IterationSet" {
            // The setting's own `is repr('VMHash')` class (`IterationSet`,
            // what core modules such as `Telemetry` build lookup tables from).
            Self::VmHash
        } else if vm_array {
            Self::VmArray
        } else if name.starts_with("array[")
            || name == "array"
            || matches!(name, "Map" | "Hash" | "List" | "Array")
            || crate::runtime::utils::is_buf_or_blob_class(name)
        {
            Self::New
        } else {
            Self::Create
        }
    }
}

/// What a `CREATE` of one registered class starts from.
#[derive(Clone)]
pub(crate) struct CreateShape {
    /// Every declared attribute seeded with its type-default empty value
    /// (`NativeCtorPlan::create_slots`).
    template: Arc<AttrMap>,
    /// An `is Hash` / `is Map` subclass, which also needs a reserved backing
    /// store even when it is allocated without `new`.
    associative: bool,
    /// An `is IterationBuffer` subclass: nqp code pushes onto `self`, so the
    /// element storage is allocated up front.
    iteration_buffer: bool,
}

/// Per-type `nqp::create` answers, valid for one registry write generation.
#[derive(Default)]
pub(crate) struct CreateMemo {
    generation: u64,
    kinds: rustc_hash::FxHashMap<Symbol, CreateKind>,
    shapes: rustc_hash::FxHashMap<Symbol, CreateShape>,
}

impl CreateMemo {
    /// Drop every answer recorded under an older generation.
    // Cost: O(1) when current; O(e) on a generation change, e = entries dropped.
    fn sync(&mut self, generation: u64) {
        if self.generation != generation {
            self.generation = generation;
            self.kinds.clear();
            self.shapes.clear();
        }
    }
}

impl Interpreter {
    /// `nqp::create($type)`: allocate an instance of `$type` with no
    /// constructor run -- attributes stay unset and `BUILD` is never called,
    /// which is Raku's `.CREATE`. nqp code uses it both for a native array
    /// (`nqp::create(array[uint32])`) and to hand-build an iterator
    /// (`nqp::create(self)` followed by `bindattr`), so it must not go
    /// anywhere near `new` except for the types [`CreateKind::New`] names.
    // Cost: O(a), a = attributes of the class (the cached slot template is copied); a
    // `New` type costs its `.new`.
    pub(crate) fn nqp_create(&mut self, ty: Value) -> Result<Value, RuntimeError> {
        if let ValueView::Package(sym) = ty.view()
            && let Some(err) = self.uninstantiable_error(sym.as_str())
        {
            return Err(err);
        }
        // A mixin type object keeps its roles (#11209).
        if let Some(result) = self.nqp_create_mixin(&ty) {
            return result;
        }
        // An instance operand creates its class, as MoarVM's `create` takes
        // the operand's type.
        let ty = match ty.view() {
            ValueView::Instance { class_name, .. } => Value::package(class_name),
            _ => ty,
        };
        let (name, kind): (&'static str, CreateKind) = match ty.view() {
            ValueView::Package(sym) => (sym.as_str(), self.create_kind_memo(sym)),
            _ => {
                let name = crate::runtime::utils::value_type_name(&ty);
                (
                    name,
                    self.create_kind(
                        name,
                        crate::qualified::last_segment(crate::qualified::known_symbol(name))
                            .as_str(),
                    ),
                )
            }
        };
        match kind {
            CreateKind::Uni => {
                let form = if name == "Uni" {
                    String::new()
                } else {
                    name.to_string()
                };
                Ok(Value::uni_from_codepoints(form, std::iter::empty()))
            }
            CreateKind::VmHash => Ok(Value::hash_with_data(Value::hash_arc(ValueMap::default()))),
            // List-kind: a VMArray, not a high-level Array (see `nqp_backing`).
            CreateKind::VmArray => Ok(Value::array(Vec::new())),
            CreateKind::CArray => self.create_carray_instance(Symbol::intern(name), &ty),
            CreateKind::New => self.call_method_with_values(ty, "new", vec![]),
            // Allocate directly rather than through `call_method_with_values`,
            // whose resolution walk before its own `CREATE` arm cost ~20K
            // instructions a call (#9122).
            CreateKind::Create => {
                let instance = match self.dispatch_create(&ty) {
                    Some(result) => result?,
                    None => self.call_method_with_values(ty, "CREATE", vec![])?,
                };
                // A bare REPR allocation: an `is repr('CStruct')` object gets
                // its zeroed native body (ADR-11209).
                self.install_cstruct_storage(&instance);
                Ok(instance)
            }
        }
    }

    /// The error constructing an instance of `class_name` raises when it was
    /// declared `is repr('Uninstantiable')` (upstream NativeCall's `void`).
    // Cost: O(n), n = chars of the name (one hash probe, skipped while no
    // such class exists).
    pub(crate) fn uninstantiable_error(&self, class_name: &str) -> Option<RuntimeError> {
        let reg = self.registry();
        (!reg.uninstantiable_classes.is_empty() && reg.uninstantiable_classes.contains(class_name))
            .then(|| RuntimeError::constrained_type_instantiation(class_name))
    }

    /// [`CreateKind`] for the type named `name` (short name `short`).
    // Cost: O(n), n = chars of the name (two hash probes of the short name); a VM storage
    // class adds an O(m) scan, m = registered method entries, memoized per generation.
    fn create_kind(&self, name: &str, short: &str) -> CreateKind {
        let reg = self.registry();
        // A VM storage class that declares methods is not a bare store: its
        // instance must dispatch them (`nqp::create(self)!SET-SELF: ...`,
        // ValueList/Tuple), so it is allocated as a real instance.
        let owner = Symbol::intern(name);
        let is_storage = reg.vmhash_classes.contains(short) || reg.vmarray_classes.contains(short);
        let bare = !is_storage
            || !reg
                .method_entries
                .iter()
                .any(|(key, entry)| key.owner == owner && !entry.user_candidates.is_empty());
        let kind = CreateKind::of(
            name,
            bare && reg.vmhash_classes.contains(short),
            bare && reg.vmarray_classes.contains(short),
        );
        drop(reg);
        if kind == CreateKind::Create && self.is_carray_repr_class(name) {
            return CreateKind::CArray;
        }
        kind
    }

    /// [`Self::create_kind`] of type object `sym`, memoized per type.
    // Cost: O(1) on a hit; a miss is O(n), n = chars of the name.
    fn create_kind_memo(&mut self, sym: Symbol) -> CreateKind {
        let generation = self.registry_write_generation();
        self.caches.create_memo.sync(generation);
        if let Some(kind) = self.caches.create_memo.kinds.get(&sym) {
            return *kind;
        }
        let short = crate::qualified::unqualified_part(sym);
        let kind = self.create_kind(sym.as_str(), short.as_str());
        // Reading the registry writes nothing, but keep the rule every memo
        // here follows: an answer computed across a write is not recorded.
        if self.registry_write_generation() == generation {
            self.caches.create_memo.kinds.insert(sym, kind);
        }
        kind
    }

    /// The `CREATE` of registered-or-not class `class`: a bare instance with
    /// every declared attribute slot present, in its type-default empty
    /// state. It does NOT run default-value expressions or BUILD/TWEAK --
    /// those belong to `bless` / `BUILDALL`. Allocating the slots is what
    /// makes a later `$!attr = ...` (in a `self.CREATE!SET-SELF: ...` private
    /// builder, as MIME::Types uses) persist: the attribute write-back only
    /// updates keys that already exist on the instance.
    // Cost: O(a), a = attributes of the class (the template is copied); a first call per
    // class and registry generation adds the constructor plan and an O(d) MRO walk, d =
    // MRO depth.
    pub(crate) fn create_instance(&mut self, class: Symbol) -> Value {
        let generation = self.registry_write_generation();
        self.caches.create_memo.sync(generation);
        let shape = match self.caches.create_memo.shapes.get(&class) {
            Some(shape) => shape.clone(),
            None => {
                let template = self.native_ctor_plan(class).create_slots.clone();
                let associative = self
                    .class_mro(class.as_str())
                    .iter()
                    .any(|name| Self::is_associative_base(name.as_str()));
                let iteration_buffer = self
                    .class_mro(class.as_str())
                    .iter()
                    .any(|name| name.as_str() == "IterationBuffer");
                let shape = CreateShape {
                    template,
                    associative,
                    iteration_buffer,
                };
                // Building the plan or the MRO may cache into the registry;
                // an answer computed across that write is not recorded.
                if self.registry_write_generation() == generation {
                    self.caches.create_memo.shapes.insert(class, shape.clone());
                }
                shape
            }
        };
        let mut attributes = (*shape.template).clone();
        // The template's `@`/`%` slots hold shared empty containers; every
        // instance needs its own.
        let containers: Vec<Symbol> = attributes
            .iter()
            .filter(|(_, v)| match v.view() {
                ValueView::Array(_, kind) => kind.is_real_array(),
                ValueView::Hash(_) => true,
                _ => false,
            })
            .map(|(k, _)| *k)
            .collect();
        for key in containers {
            let Some(template) = attributes.get(key).cloned() else {
                continue;
            };
            let fresh = if matches!(template.view(), ValueView::Hash(_)) {
                Value::hash(ValueMap::default())
            } else {
                let arr = Value::real_array(Vec::new());
                match self.container_type_metadata(&template) {
                    Some(info) => self.tag_container_metadata(arr, info),
                    None => arr,
                }
            };
            // A fresh copy of the seed is still the seed.
            attributes.rewrite(key, fresh);
        }
        if shape.associative {
            // An `is Hash`/`is Map` subclass stores its entries in a reserved
            // backing value even when it is allocated by `nqp::create` (which
            // deliberately skips `new`). Without this slot, nqp code that
            // binds `'$!storage'` while blessing the object has nowhere to
            // install the store.
            attributes.insert(
                "__mutsu_hash_storage",
                self.associative_base_storage(class.as_str(), Vec::new()),
            );
        }
        if shape.iteration_buffer {
            attributes.insert(
                super::nqp_ops_list::iteration_buffer_items_key(),
                Value::real_array(Vec::new()),
            );
        }
        Value::make_instance(class, attributes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_kind_follows_the_type_name() {
        assert_eq!(CreateKind::of("NFD", false, false), CreateKind::Uni);
        assert_eq!(CreateKind::of("IB", true, false), CreateKind::VmHash);
        assert_eq!(CreateKind::of("IB", false, true), CreateKind::VmArray);
        assert_eq!(
            CreateKind::of("array[uint32]", false, false),
            CreateKind::New
        );
        assert_eq!(CreateKind::of("Hash", false, false), CreateKind::New);
        assert_eq!(CreateKind::of("IB", false, false), CreateKind::Create);
    }
}
