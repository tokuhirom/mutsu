use super::*;
use crate::runtime::meta_ns::MetaNs;
use std::borrow::Cow;
use std::cell::RefCell;

/// The qualified private-attribute keys of one `(owner, bare, sigil)`:
/// `Owner\0<sigil>bare` (only present for a sigil-colliding attribute) and
/// `Owner\0bare` (a private attribute declared in both a parent and a child).
#[derive(Clone, Copy)]
struct QualifiedAttrKeys {
    with_sigil: crate::symbol::Symbol,
    plain: crate::symbol::Symbol,
}

thread_local! {
    /// Memo of [`QualifiedAttrKeys`] per `(owner, bare, sigil)`. The keys are a
    /// pure function of three interned names, but building them meant
    /// assembling a string and interning it (a string hash plus a table probe)
    /// twice on every private `$!x` access (ADR-0121 D1).
    static QUALIFIED_ATTR_KEYS: RefCell<
        rustc_hash::FxHashMap<(crate::symbol::Symbol, crate::symbol::Symbol, char), QualifiedAttrKeys>,
    > = RefCell::new(rustc_hash::FxHashMap::default());
    /// Memo of the sigil-prefixed storage key (`%bare`) per `(bare, sigil)`,
    /// for the same reason: it used to be a `format!` plus an intern per miss.
    static SIGIL_ATTR_KEYS: RefCell<
        rustc_hash::FxHashMap<(crate::symbol::Symbol, char), crate::symbol::Symbol>,
    > = RefCell::new(rustc_hash::FxHashMap::default());
}

impl Interpreter {
    /// Assign a single value into a stack-computed Hash/Array `target` at `key`
    /// via interior mutation, writing through an existing `:=`-bound
    /// `ContainerRef` cell. Used by junction/slice autothreading in the generic
    /// index-assign op, where the target is a resolved inner container reached
    /// through a nested subscript (`%h<x>{...}`).
    pub(crate) fn assign_into_computed_target(
        &self,
        target: &Value,
        key: &Value,
        val: Value,
    ) -> Result<(), RuntimeError> {
        match target.view() {
            ValueView::Hash(arc) => {
                let k = Value::hash_key_encode(key);
                // SAFETY: aliased in-place mutation of a shared hash; see
                // `gc_contents_mut`. No live borrow into the map.
                let hd = unsafe { crate::value::gc_contents_mut(&arc) };
                Value::hash_insert_through(&mut hd.map, k, val);
            }
            ValueView::Array(arc, _) => {
                if let Some(i) = Self::index_to_usize(key) {
                    // SAFETY: aliased in-place mutation of a shared array; see
                    // `gc_contents_mut`.
                    let data = unsafe { crate::value::gc_contents_mut(&arc) };
                    let old_len = data.items().len();
                    // ADR-0049 slice 5: fill skipped slots with the standard
                    // `Package("Any")` gap marker (matching every other
                    // autoviv-resize call site) instead of a raw `Value::NIL`
                    // -- `Nil` is no longer a hole sentinel, only
                    // `ArrayData::initialized` is.
                    Self::autoviv_resize(
                        data.items_mut(),
                        i + 1,
                        Self::native_fill_for_constraint(None),
                    )?;
                    Value::assign_element_slot(&mut data.items_mut()[i], val);
                    // Materialize the "all present" range (`None` means every
                    // in-range index exists) before recording `i` as present,
                    // so a skipped intermediate slot from the resize above is
                    // correctly left OUT and reads as a gap via `hole_at`.
                    data.initialized
                        .get_or_insert_with(|| (0..old_len).collect())
                        .insert(i);
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Phase 2 phantom-entry: materialize a missing-key `:=` bind on the first
    /// write through the bound variable. A local holding a `HashEntryRef` deferred
    /// token (single missing key `$e := %m<solo>`, or a multi-key path
    /// `$d := %k<p><q>`) is converted into a shared `ContainerRef` cell: the path
    /// is walk-created (`hash_entry_terminal`) and the cell is installed at the
    /// terminal hash entry, and the local is replaced with the same cell. After
    /// this the bound var and the hash entry alias bidirectionally — the old
    /// plain-value materialization lost the alias so a later cross-write was not
    /// observed (the case-C bug). Returns `true` when it handled `idx`; the
    /// deferred token is unchanged until first write, so `:exists` and pre-write
    /// reads keep their lazy semantics.
    pub(super) fn materialize_bound_slot_to_cell(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        val: Value,
    ) -> Result<bool, RuntimeError> {
        let cell = if matches!(self.locals[idx].view(), ValueView::HashEntryRef { .. }) {
            let token = self.locals[idx].clone();
            // Walk-create the deferred path (single key for `$e := %m<solo>`,
            // multi-key for `$d := %k<p><q>`) and install the shared cell at
            // the terminal entry so the bound var and the hash entry alias
            // bidirectionally afterwards.
            let Some(terminal) = token.hash_entry_terminal() else {
                return Ok(false);
            };
            self.materialize_entry_cell(&terminal, val)?
        } else {
            return Ok(false);
        };
        let cell_val = Value::container_ref(cell);
        self.locals[idx] = cell_val.clone();
        self.flush_local_to_env(code, idx);
        // A `:=` bind also registers a sigilless alias (`__mutsu_sigilless_alias::$x`
        // -> `__mutsu_bind_index_ref_N`), and the env-centric element-assign
        // handlers redirect through it before they look at anything else. That
        // alias target still held the pre-materialization value, so a SECOND
        // write through the bound variable (`$x = ['a']; $x[1] = 'b'`) found no
        // container there, autovivified a fresh one, and silently detached from
        // the hash entry. Point the alias at the cell too.
        if let Some(name) = code.locals.get(idx).cloned() {
            let root = self.resolve_alias_root(&name);
            if root != name {
                self.set_env_with_main_alias(&root, cell_val);
            }
        }
        Ok(true)
    }

    // --- Phase 3 Stage 2: scalar instance attributes as cell-direct (slice 1) ---
    //
    // For scalar attribute-twigil locals (`$!x` -> `!x`, `$.x` -> `.x`) the
    // instance's shared attribute cell is the single source of truth. Reads come
    // straight from the cell (so a mutation made in a nested method frame is
    // visible to the caller — the cross-frame bug), and every write mirrors the
    // local back into the cell. This lets the scalar writeback be dropped
    // (`writeback_attributes*` skip scalar attrs). Array/hash attributes still
    // use the materialize+writeback path for now (later slices).

    /// If `name` is an attribute-twigil local — scalar (`!x`/`.x`), array
    /// (`@!x`/`@.x`) or hash (`%!x`/`%.x`) — return `(bare attribute name,
    /// is_private)`. Excludes special vars (`!`, `.`) and internal names. The
    /// cell stores attributes under the bare name, so all six twigil forms of an
    /// attribute resolve to the same cell slot.
    pub(crate) fn attr_twigil_base(name: &str) -> Option<(&str, bool)> {
        crate::value::attr_twigil_base(name)
    }

    /// The qualified private-attribute keys of `bare` as declared by `owner`,
    /// memoized (see [`QualifiedAttrKeys`]).
    // Cost: O(1) amortized: one hash probe of three interned ids; the first
    // ask per (owner, bare, sigil) interns two strings of O(len) each.
    fn qualified_attr_keys(
        owner: crate::symbol::Symbol,
        bare: crate::symbol::Symbol,
        sigil: char,
    ) -> QualifiedAttrKeys {
        QUALIFIED_ATTR_KEYS.with(|memo| {
            *memo
                .borrow_mut()
                .entry((owner, bare, sigil))
                .or_insert_with(|| {
                    let (owner, bare) = (owner.as_str(), bare.as_str());
                    QualifiedAttrKeys {
                        with_sigil: crate::symbol::Symbol::intern(&format!(
                            "{owner}\0{sigil}{bare}"
                        )),
                        plain: crate::symbol::Symbol::intern(&format!("{owner}\0{bare}")),
                    }
                })
        })
    }

    /// The sigil-prefixed storage key of `bare` (`%bare`), memoized.
    // Cost: O(1) amortized, as `qualified_attr_keys`.
    fn sigil_attr_key(bare: crate::symbol::Symbol, sigil: char) -> crate::symbol::Symbol {
        SIGIL_ATTR_KEYS.with(|memo| {
            *memo.borrow_mut().entry((bare, sigil)).or_insert_with(|| {
                crate::symbol::Symbol::intern(&format!("{sigil}{}", bare.as_str()))
            })
        })
    }

    /// Pick the cell key actually present in `map` for the attribute `(bare,
    /// is_private)`, preferring the method owner class's qualified private key
    /// when present (Parent/Child same-named `$!priv` disambiguation), matching
    /// the order used when method frames materialize attributes. `None` when the
    /// attribute does not exist in the cell. `owner` is the running method's
    /// owner (the top of the method-class stack).
    // Cost: O(1): at most four hash probes of the attribute map.
    fn attr_key_in_map(
        owner: Option<crate::symbol::Symbol>,
        bare: crate::symbol::Symbol,
        is_private: bool,
        sigil: char,
        map: &crate::value::AttrMap,
    ) -> Option<crate::symbol::Symbol> {
        if is_private && let Some(owner) = owner {
            let keys = Self::qualified_attr_keys(owner, bare, sigil);
            if map.contains_key(keys.with_sigil) {
                return Some(keys.with_sigil);
            }
            if map.contains_key(keys.plain) {
                return Some(keys.plain);
            }
        }
        if map.contains_key(bare) {
            Some(bare)
        } else {
            let sigil_key = Self::sigil_attr_key(bare, sigil);
            map.contains_key(sigil_key).then_some(sigil_key)
        }
    }

    /// `self` for an attribute access made by `code`'s frame. A method body's
    /// invocant is its first local slot (`self`), so reading it from there
    /// skips the by-name env resolution (`get_env_self`), which walks the
    /// unit-scope, package and module-lexical redirects before the env probe on
    /// every access. Any other frame -- a closure or a nested block inside the
    /// method, a sub, the mainline -- and a slot that does not hold an
    /// object, resolves it by name as before.
    // Cost: O(1) for a method body; get_env_self's cost otherwise.
    pub(super) fn attr_access_self(&self, code: &CompiledCode) -> Option<Value> {
        if code.locals_sym.first() == Some(&crate::symbol::wk::self_())
            && let Some(v) = self.locals.first()
            && matches!(
                v.view(),
                ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::ContainerRef(_)
            )
        {
            return Some(v.clone());
        }
        self.get_env_self()
    }

    /// The inner instance's shared attribute cell for a `self` value, unwrapping a
    /// `Mixin` (runtime `$obj does Role`) to the wrapped instance. The
    /// Mixin's inner value is held in a shared `Arc`, and the instance's own cell is
    /// an `Arc<RwLock>` — so a write through this reference persists back to the
    /// caller's Mixin (it shares the same inner instance). A `ContainerRef` is read
    /// through: `$outer := self` rewrites the frame's `self` into the bind's shared
    /// cell, and attribute writes after that bind must still reach the instance
    /// (t/bind-self-attr-write.t). Returns `None` for a type object / non-instance.
    ///
    /// The unwrap is iterative and bounded, and each `ContainerRef` level is
    /// cloned out via `deref_container` so NO cell lock is held while looking at
    /// the next level — a recursive deref inside `with_deref` would hold the
    /// Mutex across levels, turning a pathological cell cycle into a same-thread
    /// re-lock (deadlock) or unbounded stack growth. A chain deeper than the cap
    /// yields `None`, exactly like a non-instance.
    pub(crate) fn self_instance_attrs(
        val: &Value,
    ) -> Option<crate::gc::Gc<crate::value::InstanceAttrs>> {
        let mut cur: Option<Value> = None;
        for _ in 0..8 {
            let v = cur.as_ref().unwrap_or(val);
            let next = match v.view() {
                ValueView::Instance { attributes, .. } => return Some(attributes.clone()),
                ValueView::Mixin(inner, _) => Value::clone(inner),
                ValueView::ContainerRef(_) => v.deref_container(),
                _ => return None,
            };
            cur = Some(next);
        }
        None
    }

    /// Return the attribute stores visible to a method, in lookup order. A
    /// role method on a Mixin owns the Mixin's role cell; attributes not found
    /// there fall through to the wrapped instance cell. Methods owned by the
    /// wrapped class see the ordinary instance cell only.
    pub(crate) fn method_attr_cells(
        &self,
        val: &Value,
        owner: &str,
    ) -> (
        Option<crate::gc::Gc<crate::value::InstanceAttrs>>,
        Option<crate::gc::Gc<crate::value::InstanceAttrs>>,
    ) {
        Self::method_attr_cells_for(val, owner, self.is_role(owner))
    }

    /// [`Self::method_attr_cells`] for a caller that already knows whether
    /// `owner` is a role (the running method's frame memoizes it).
    fn method_attr_cells_for(
        val: &Value,
        owner: &str,
        owner_is_role: bool,
    ) -> (
        Option<crate::gc::Gc<crate::value::InstanceAttrs>>,
        Option<crate::gc::Gc<crate::value::InstanceAttrs>>,
    ) {
        let inner = Self::self_instance_attrs(val);
        if !owner_is_role {
            return (None, inner);
        }
        let mut current = val.clone();
        for _ in 0..8 {
            match current.view() {
                ValueView::Mixin(inner_value, mixins) => {
                    let marker = MetaNs::Role.owned_key_for_str(owner);
                    if mixins.contains_key(&marker) {
                        return (Some(mixins.attributes().clone()), inner);
                    }
                    current = inner_value.as_ref().clone();
                }
                ValueView::ContainerRef(_) => {
                    current = current.deref_container();
                }
                _ => break,
            }
        }
        (None, inner)
    }

    /// Select the primary live attribute cell for a resolved method owner.
    /// Per-key reads and writes additionally fall through from a role cell to
    /// the wrapped instance cell.
    pub(crate) fn method_attr_cell(
        &self,
        val: &Value,
        owner: &str,
    ) -> Option<crate::gc::Gc<crate::value::InstanceAttrs>> {
        let (role, inner) = self.method_attr_cells(val, owner);
        role.or(inner)
    }

    /// The role-cell key of `bare` for the role `owner`, found by walking
    /// `val`'s mixin layers; `None` when no layer composes `owner`.
    fn role_attr_key_in_mixin(
        val: &Value,
        owner: &str,
        bare: crate::symbol::Symbol,
    ) -> Option<crate::symbol::Symbol> {
        let mut current = val.clone();
        for _ in 0..8 {
            match current.view() {
                ValueView::Mixin(inner_value, mixins) => {
                    let marker = MetaNs::Role.owned_key_for_str(owner);
                    if mixins.contains_key(&marker) {
                        return Some(mixins.role_attribute_key(owner, bare.as_str()));
                    }
                    current = inner_value.as_ref().clone();
                }
                ValueView::ContainerRef(_) => current = current.deref_container(),
                _ => return None,
            }
        }
        None
    }

    /// Read a scalar attribute straight from `self`'s shared cell. `Some` only
    /// when `name` is a scalar attr-twigil, `self` is a concrete instance (or a
    /// Mixin wrapping one), and the attribute exists in the cell.
    pub(crate) fn read_self_attr_cell(&self, name: &str) -> Option<Value> {
        let twigil = self.canonical_attr_twigil(name)?;
        let (bare, is_private) = Self::attr_twigil_base(&twigil)?;
        let sigil = crate::value::attr_twigil_sigil(&twigil).unwrap_or('$');
        self.read_attr_cell_by_key(
            self.get_env_self(),
            None,
            crate::symbol::Symbol::intern(bare),
            is_private,
            sigil,
        )
    }

    /// Slot form of [`Self::read_self_attr_cell`]: the attribute `Symbol` comes
    /// pre-resolved from the chunk's local-slot table, so the hot `$!x` / `$.x`
    /// read parses no twigil, interns no string and allocates nothing. Falls back
    /// to the name-keyed path only for sigilless attributes (`has $x`), whose
    /// alias must be followed through the runtime alias table.
    pub(super) fn read_self_attr_cell_slot(
        &self,
        code: &CompiledCode,
        idx: usize,
    ) -> Option<Value> {
        match code.local_attr_key(idx) {
            Some((bare, is_private, sigil)) => {
                let self_val = self.attr_access_self(code);
                if let Some(sv) = &self_val
                    && let Some(v) = self.read_attr_site_cached(code, idx, sv)
                {
                    return Some(v);
                }
                self.read_attr_cell_by_key(self_val, Some((code, idx)), bare, is_private, sigil)
            }
            None => {
                if !self.sigilless_attrs_active {
                    return None;
                }
                self.read_self_attr_cell(code.locals.get(idx)?)
            }
        }
    }

    /// Resolve the attribute `(bare, is_private, sigil)` against `self_val`'s
    /// live cells for the running method -- a role method on a Mixin looks in
    /// the Mixin's role cell first, then everything falls through to the
    /// wrapped instance cell -- and hand `f` the cell, its map (under the read
    /// guard) and the key the attribute is stored under. `None` when neither
    /// cell holds it. The one resolution the read, write and in-place append
    /// paths share.
    // Cost: O(m), m = mixin layers of `self_val` (bounded by 8); O(1) for a
    // plain instance: a few attribute-map probes, no registry access.
    fn with_self_attr<R>(
        &self,
        self_val: &Value,
        site: Option<(&CompiledCode, usize)>,
        bare: crate::symbol::Symbol,
        is_private: bool,
        sigil: char,
        f: impl FnOnce(
            &crate::gc::Gc<crate::value::InstanceAttrs>,
            &crate::value::AttrMap,
            crate::symbol::Symbol,
        ) -> R,
    ) -> Option<R> {
        let owner = self.method_class_stack_top_sym();
        let owner_str = owner.map_or("", |o| o.as_str());
        let (role_cell, inner_cell) =
            Self::method_attr_cells_for(self_val, owner_str, self.method_class_top_is_role());
        if let Some(attributes) = role_cell {
            let map = attributes.as_map();
            let key = Self::role_attr_key_in_mixin(self_val, owner_str, bare)
                .filter(|key| map.contains_key(*key))
                .or_else(|| Self::attr_key_in_map(owner, bare, is_private, sigil, &map));
            if let Some(key) = key {
                return Some(f(&attributes, &map, key));
            }
        }
        if let Some(attributes) = inner_cell {
            let map = attributes.as_map();
            if let Some(key) = Self::attr_key_in_map(owner, bare, is_private, sigil, &map) {
                if let Some((code, idx)) = site {
                    Self::fill_attr_site(code, idx, &map, key, bare, is_private);
                }
                return Some(f(&attributes, &map, key));
            }
        }
        None
    }

    /// The shared tail of both read paths: resolve `(bare, is_private)` against
    /// `self`'s live cell under a single read guard and clone the value out.
    fn read_attr_cell_by_key(
        &self,
        self_val: Option<Value>,
        site: Option<(&CompiledCode, usize)>,
        bare: crate::symbol::Symbol,
        is_private: bool,
        sigil: char,
    ) -> Option<Value> {
        if let Some(self_val) = self_val
            && let Some(found) =
                self.with_self_attr(&self_val, site, bare, is_private, sigil, |_, map, key| {
                    map.get(key).map(|v| v.deref_container())
                })
        {
            return found;
        }
        self.read_class_level_attr_cell(bare, is_private)
    }

    /// Fall back to a class-level attribute (`my $.x` / `our $.x`) when the
    /// instance-cell lookup above found nothing — either because `self` is a
    /// type object (`Foo.imm`, no instance at all) or because the bare name is
    /// simply not one of `self`'s per-instance attributes. A class-level
    /// attribute is never stored in any instance's cell; it lives in exactly
    /// one place, `ClassDef::class_level_attrs` on the declaring class, which
    /// `get_class_level_attr` reads directly through the shared registry (no
    /// per-call env mirror — see ADR-0013/ADR-0039's "one canonical cell"
    /// discipline). Only public (`.`-twigil) attributes ever reach
    /// `class_level_attrs` — a `!`-twigil is a parse error on `my`/`our` — so a
    /// private lookup can never legitimately land here.
    fn read_class_level_attr_cell(
        &self,
        bare: crate::symbol::Symbol,
        is_private: bool,
    ) -> Option<Value> {
        if is_private {
            return None;
        }
        let owner = self.method_class_stack_top_str()?;
        self.get_class_level_attr(owner, bare.as_str())
    }

    /// The instance class `Symbol` and shared attribute cell for a `self` value,
    /// unwrapping a `Mixin` like [`Self::self_instance_attrs`]. `None` for a
    /// type object / non-instance.
    fn instance_class_and_attrs(
        val: &Value,
    ) -> Option<(
        crate::symbol::Symbol,
        crate::gc::Gc<crate::value::InstanceAttrs>,
    )> {
        match val.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => Some((class_name, attributes.clone())),
            ValueView::Mixin(inner, _) => Self::instance_class_and_attrs(inner),
            _ => None,
        }
    }

    /// Rakudo parity (weird-errors.t test 29): reading a private attribute
    /// (`$!x`) on a concrete invocant whose class neither carries the attribute
    /// in its cell nor declares it anywhere in its MRO throws the P6opaque
    /// no-such-attribute error instead of yielding Nil — e.g. an
    /// `our method foo(Parent:)` reading a Child-only `$!x`, called with a
    /// Parent instance. `None` when the read may legally fall through: no
    /// `self` in scope, a type-object invocant, the attribute present in the
    /// cell, or declared on the instance's class (a seeding gap must not turn
    /// into a spurious throw).
    pub(super) fn missing_private_attr_read_error(&mut self, name: &str) -> Option<RuntimeError> {
        let (bare, is_private) = Self::attr_twigil_base(name)?;
        let sigil = crate::value::attr_twigil_sigil(name).unwrap_or('$');
        if !is_private {
            return None;
        }
        let self_val = self.get_env_self()?;
        let (class_sym, attributes) = Self::instance_class_and_attrs(&self_val)?;
        {
            let map = attributes.as_map();
            if Self::attr_key_in_map(
                self.method_class_stack_top_sym(),
                crate::symbol::Symbol::intern(bare),
                true,
                sigil,
                &map,
            )
            .is_some()
            {
                return None;
            }
        }
        let class_name = class_sym.as_str();
        if self.class_declares_attribute(class_name, bare) {
            return None;
        }
        let owner = self
            .method_class_stack_top_str()
            .unwrap_or(class_name)
            .to_string();
        Some(RuntimeError::new(format!(
            "P6opaque: no such attribute '$!{bare}' on type {owner} in a {class_name} when trying to get a value"
        )))
    }

    /// Map a variable name to its canonical attribute-twigil form for cell access:
    /// a direct twigil (`!x`/`@.y`/…) maps to itself; a bare sigilless name
    /// (`has $x` → `Var("x")`) resolves through the runtime alias table to its
    /// `!x` twigil. Returns `None` for ordinary (non-attribute) names. The
    /// sigilless lookup is gated on `sigilless_attrs_active` so the common case
    /// (no sigilless attributes) costs only a string check on the hot read path.
    pub(crate) fn canonical_attr_twigil<'n>(&self, name: &'n str) -> Option<Cow<'n, str>> {
        if Self::attr_twigil_base(name).is_some() {
            // The overwhelmingly common case: the name already *is* the twigil.
            // Borrow it — this used to allocate a `String` on every attribute read.
            return Some(Cow::Borrowed(name));
        }
        if !self.sigilless_attrs_active {
            return None;
        }
        self.sigilless_attr_twigil(name).map(Cow::Owned)
    }

    /// Follow the `__mutsu_sigilless_alias::` chain from a bare sigilless name
    /// until it reaches an attribute twigil (`!x`), returning that twigil. The
    /// alias table is bidirectional (`x ↔ !x`), so the `seen` guard prevents a
    /// cycle; returns `None` if the chain has no attribute-twigil link.
    pub(crate) fn sigilless_attr_twigil(&self, name: &str) -> Option<String> {
        let mut current = name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(current.clone()) {
            let key = crate::runtime::sigilless_alias_key(&current);
            match self.env().get_sym(key).map(Value::view) {
                Some(ValueView::Str(next)) => {
                    let next = next.to_string();
                    if Self::attr_twigil_base(&next).is_some() {
                        return Some(next);
                    }
                    current = next;
                }
                _ => return None,
            }
        }
        None
    }

    /// Skip mirroring slot values that keep their legacy handling (`:=` bindings,
    /// Proxy accessors, hash/array slot refs, deferred hash access).
    pub(crate) fn is_non_mirrorable_attr_value(val: &Value) -> bool {
        matches!(
            val.view(),
            ValueView::ContainerRef(_) | ValueView::Proxy { .. } | ValueView::HashEntryRef { .. }
        )
    }

    /// Write `val` into `self`'s shared cell for the scalar attribute named
    /// `name` (`!x`/`.x`), resolving the qualified private key when present.
    /// No-op when `name` is not a scalar attr-twigil, `self` is not a concrete
    /// instance, or the attribute does not exist on `self`.
    pub(crate) fn write_self_attr_cell(&self, name: &str, val: Value) {
        let Some((bare, is_private)) = Self::attr_twigil_base(name) else {
            return;
        };
        let sigil = crate::value::attr_twigil_sigil(name).unwrap_or('$');
        self.write_attr_cell_by_key(
            self.get_env_self(),
            None,
            crate::symbol::Symbol::intern(bare),
            is_private,
            sigil,
            val,
        );
    }

    /// The shared tail of both write paths. Unwraps a `Mixin` self to the inner
    /// instance's shared cell so a runtime-`does` mixin method's `$.attr`/`$!attr`
    /// write persists (the cell is an `Arc<RwLock>` shared with the caller's
    /// Mixin). No-op when `self` is not a concrete instance or the attribute does
    /// not exist on it.
    fn write_attr_cell_by_key(
        &self,
        self_val: Option<Value>,
        site: Option<(&CompiledCode, usize)>,
        bare: crate::symbol::Symbol,
        is_private: bool,
        sigil: char,
        val: Value,
    ) {
        // The key is resolved under the read guard, which is released before
        // the store takes the write lock.
        if let Some(self_val) = self_val
            && let Some((attributes, key)) = self.with_self_attr(
                &self_val,
                site,
                bare,
                is_private,
                sigil,
                |attributes, _, key| (attributes.clone(), key),
            )
        {
            self.record_build_attr_write(&attributes, key);
            attributes.store_through_container(key, val);
            return;
        }
        // Class-level attribute fallback: see `read_class_level_attr_cell`'s
        // doc comment for why this is the canonical (not a mirrored) store.
        if is_private {
            return;
        }
        let Some(owner) = self.method_class_stack_top_str() else {
            return;
        };
        self.set_class_level_attr(owner, bare.as_str(), val);
    }

    /// `$!x ~= <Str>` appended in place: `lhs` is the value moved out of the
    /// attribute's local slot, and `self`'s cell for the attribute must hold
    /// the very same string. Under the attribute map's write lock the cell's
    /// reference is dropped, so `lhs` is the buffer's only holder and the
    /// append grows it (amortized O(m)) instead of copying it (#9209); the
    /// result is stored back into the cell and returned for the slot. Gives
    /// `lhs` back untouched when the cell holds anything else (a promoted
    /// `ContainerRef`, a different value) -- the caller then takes the general
    /// path. Resolves the attribute exactly as [`Self::write_attr_cell_by_key`]
    /// does.
    // Cost: amortized O(m), m = chars of the suffix; O(n + m) when the string
    // is still shared elsewhere (the append copies), n = chars accumulated.
    pub(super) fn append_attr_cell_str_in_place(
        &self,
        bare: crate::symbol::Symbol,
        is_private: bool,
        sigil: char,
        lhs: Value,
        plan: &crate::value::StrAppendPlan<'_>,
    ) -> Result<Value, Value> {
        let Some(self_val) = self.get_env_self() else {
            return Err(lhs);
        };
        let Some((attributes, key)) = self.with_self_attr(
            &self_val,
            None,
            bare,
            is_private,
            sigil,
            |attributes, _, key| (attributes.clone(), key),
        ) else {
            return Err(lhs);
        };
        let mut held = Some(lhs);
        let appended = attributes
            .with_attr_mut(key, |slot| {
                let lhs = held.take()?;
                if !matches!(slot.view(), ValueView::Str(_)) || !slot.same_binding(&lhs) {
                    held = Some(lhs);
                    return None;
                }
                *slot = Value::NIL;
                let new_val = lhs.str_appended_nfc(plan);
                *slot = new_val.clone();
                Some(new_val)
            })
            .flatten();
        match appended {
            Some(new_val) => {
                self.record_build_attr_write(&attributes, key);
                Ok(new_val)
            }
            None => Err(held.unwrap_or(Value::NIL)),
        }
    }

    /// Note that `key` was assigned on `attributes` while that instance's BUILD
    /// phase is running, so the post-BUILD default pass knows to leave it alone
    /// (raku applies a `has $.x = <default>` only to attributes BUILD did not
    /// set — an explicit `$!x = Any` counts as set). Costs one `RefCell` borrow
    /// of an empty `Vec` outside construction.
    pub(crate) fn record_build_attr_write(
        &self,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        key: crate::symbol::Symbol,
    ) {
        let mut frames = self.build_attr_writes.borrow_mut();
        if frames.is_empty() {
            return;
        }
        let addr = crate::gc::Gc::as_ptr(attributes) as usize;
        if let Some(frame) = frames.iter_mut().rev().find(|f| f.cell_addr == addr) {
            frame.written.insert(key);
        }
    }

    /// Mirror the current local slot value into `self`'s shared cell for a scalar
    /// attribute, after the normal write logic has finalized the slot. The
    /// attribute `Symbol` is pre-resolved per chunk, so a non-attribute slot (the
    /// common case) costs one table load and an attribute slot allocates nothing.
    pub(super) fn mirror_attr_local_to_cell(&self, code: &CompiledCode, idx: usize) {
        let Some((bare, is_private, sigil)) = code.local_attr_key(idx) else {
            return;
        };
        // `$!x := $y` leaves `$y`'s container in the slot: the attribute is now
        // that container, so seat the same cell in `self` (not a copy of what
        // it holds) -- otherwise `self.x` inside this method still reads the
        // old container until the method returns.
        if matches!(self.locals[idx].view(), ValueView::ContainerRef(_))
            && sigil == '$'
            && let Some(self_val) = self.attr_access_self(code)
            && let Some((attributes, key)) = self.with_self_attr(
                &self_val,
                Some((code, idx)),
                bare,
                is_private,
                sigil,
                |attributes, _, key| (attributes.clone(), key),
            )
        {
            let cell = self.locals[idx].clone();
            attributes.with_attr_mut(key, |slot| {
                if !slot.same_binding(&cell) {
                    *slot = cell;
                }
            });
            return;
        }
        if Self::is_non_mirrorable_attr_value(&self.locals[idx]) {
            return;
        }
        let self_val = self.attr_access_self(code);
        let val = match &self_val {
            Some(sv) => {
                match self.write_attr_site_cached(code, idx, sv, self.locals[idx].clone()) {
                    Ok(()) => return,
                    Err(val) => val,
                }
            }
            None => self.locals[idx].clone(),
        };
        self.write_attr_cell_by_key(self_val, Some((code, idx)), bare, is_private, sigil, val);
    }

    /// Mirror the finalized value of the variable named `name` into `self`'s
    /// shared cell, for write ops that dispatch by name (e.g. the name-based
    /// `AssignExpr`). Reads the value back from the local slot or env after the
    /// op completed.
    pub(super) fn mirror_attr_value_to_cell_by_name(&self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        let val = self
            .find_local_slot(code, name)
            .map(|slot| self.locals[slot].clone())
            .or_else(|| self.get_env_with_main_alias(name));
        let Some(val) = val else {
            return;
        };
        if Self::is_non_mirrorable_attr_value(&val) {
            return;
        }
        self.write_self_attr_cell(name, val);
    }

    /// True if `name` is an array/hash attribute twigil (`@!`/`@.`/`%!`/`%.`) —
    /// an attribute whose whole container value lives in env, so a mutating op
    /// keyed by the name operates on the env copy.
    pub(crate) fn is_array_hash_attr_twigil(name: &str) -> bool {
        (name.starts_with("@!")
            || name.starts_with("@.")
            || name.starts_with("%!")
            || name.starts_with("%."))
            && Self::attr_twigil_base(name).is_some()
    }

    /// True for the twigils a *subscript* op may target: the array/hash forms
    /// above plus the sigilless scalar forms (`!x`/`.x`), because a scalar
    /// attribute can itself hold a Hash/Array that `$!h<k> = v` mutates.
    ///
    /// Deliberately NOT used by the whole-value ops (`SetGlobal`, `CallMethodMut`,
    /// `ArrayPush`). A scalar attribute is cell-direct: those ops already reach
    /// the cell through the scalar read/write path — `$!a.push(1)` mutates the
    /// container the cell holds in place — so routing them through this env↔cell
    /// sync would buy nothing and would put a stale env copy in the path of a
    /// concurrent cell write from another thread.
    pub(crate) fn is_subscriptable_attr_twigil(name: &str) -> bool {
        Self::attr_twigil_base(name).is_some()
    }

    /// Snapshot the env/shared value of an array/hash attribute variable before a
    /// mutating op, so [`Self::mirror_attr_env_to_cell`] can tell a genuine
    /// mutation (env value changed) from a non-mutating method call (`@!a.join`)
    /// on a stale env copy — mirroring the stale copy would clobber a cross-frame
    /// cell mutation. Returns `None` for non-attribute targets (cheap fast path).
    ///
    /// Also refreshes the env/local copy from `self`'s live cell before the op
    /// runs: a closure-captured env copy is a stale snapshot from closure
    /// creation, so letting the op mutate it (and mirroring the result) would
    /// clobber keys written by earlier calls — `%!h{$k} = $v` inside a returned
    /// closure kept only the last write. After the refresh the op starts from
    /// the live cell value and the mirror's pre-snapshot is that same value.
    pub(super) fn attr_env_snapshot(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Value> {
        self.attr_env_snapshot_matching(code, name_idx, Self::is_array_hash_attr_twigil)
    }

    /// [`Self::attr_env_snapshot`] for the subscript ops, which also cover a
    /// scalar attribute holding a container. Such a slot is populated lazily by a
    /// cell-direct *read*, so an element assignment with no preceding read
    /// (`$!h<k> = 1` as the first touch in a method) would otherwise find no
    /// env/slot entry and autovivify a fresh container that never reaches the cell.
    pub(super) fn attr_elem_env_snapshot(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Value> {
        self.attr_env_snapshot_matching(code, name_idx, Self::is_subscriptable_attr_twigil)
    }

    fn attr_env_snapshot_matching(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        applies: fn(&str) -> bool,
    ) -> Option<Value> {
        let name = Self::const_str(code, name_idx);
        if !applies(name) {
            return None;
        }
        let name = name.to_string();
        let env_val = self
            .get_env_with_main_alias(&name)
            .or_else(|| self.get_shared_var(&name));
        // `:=` bindings / slot refs keep their legacy env handling.
        if env_val
            .as_ref()
            .is_some_and(Self::is_non_mirrorable_attr_value)
        {
            return env_val;
        }
        if let Some(cell_val) = self.read_self_attr_cell(&name) {
            // Env copies share the cell value's Arc until a copy-on-write fork;
            // pointer inequality means the copy is stale (or absent) — adopt the
            // live cell value so the op mutates current state.
            if !env_val
                .as_ref()
                .is_some_and(|v| Self::same_container_arc(v, &cell_val))
            {
                self.env_mut().insert(name.clone(), cell_val.clone());
                if let Some(slot) = self.find_local_slot(code, &name) {
                    self.locals[slot] = cell_val.clone();
                }
            }
            return Some(cell_val);
        }
        env_val
    }

    /// Cheap container identity check: true when both values are the same
    /// Array/Hash Arc (a clone that has not been copy-on-write forked).
    pub(crate) fn same_container_arc(a: &Value, b: &Value) -> bool {
        match (a.view(), b.view()) {
            (ValueView::Array(x, _), ValueView::Array(y, _)) => crate::gc::Gc::ptr_eq(&x, &y),
            (ValueView::Hash(x), ValueView::Hash(y)) => crate::gc::Gc::ptr_eq(&x, &y),
            _ => false,
        }
    }

    /// Mirror an array/hash attribute variable's post-mutation value into `self`'s
    /// shared cell (Phase 3 Stage 2b). Used after the mutating array/hash ops
    /// (`@!a.push`, `@!a[i]=`, `%!h<k>=`, …), which write the new container into
    /// env/shared keyed by `name`. Only fires for `@!`/`@.`/`%!`/`%.` twigils and
    /// only when the env value actually changed from `pre` (so a non-mutating
    /// method like `@!a.join` on a stale env copy does not clobber the cell).
    pub(super) fn mirror_attr_env_to_cell(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        pre: Option<Value>,
    ) {
        self.mirror_attr_env_to_cell_matching(code, name_idx, pre, Self::is_array_hash_attr_twigil);
    }

    /// [`Self::mirror_attr_env_to_cell`] for the subscript ops, which also cover
    /// a scalar attribute holding a container (`$!h<k> = v`).
    pub(super) fn mirror_attr_elem_env_to_cell(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        pre: Option<Value>,
    ) {
        self.mirror_attr_env_to_cell_matching(
            code,
            name_idx,
            pre,
            Self::is_subscriptable_attr_twigil,
        );
    }

    fn mirror_attr_env_to_cell_matching(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        pre: Option<Value>,
        applies: fn(&str) -> bool,
    ) {
        let name = Self::const_str(code, name_idx);
        if !applies(name) {
            return;
        }
        let name = name.to_string();
        // The mutating ops write the new container into env (or shared_vars).
        let val = self
            .get_env_with_main_alias(&name)
            .or_else(|| self.get_shared_var(&name));
        let Some(val) = val else {
            return;
        };
        // No env change -> either a non-mutating method or a no-op; do not write
        // a possibly-stale env copy over a cross-frame cell mutation.
        if pre.as_ref() == Some(&val) {
            return;
        }
        if Self::is_non_mirrorable_attr_value(&val) {
            return;
        }
        self.write_self_attr_cell(&name, val);
    }

    /// Refresh the local slot from `self`'s cell before a read-modify-write on a
    /// scalar attribute (increment/decrement), so the operation sees a mutation
    /// made in a nested frame rather than the materialized snapshot.
    pub(crate) fn sync_attr_local_from_cell(&mut self, code: &CompiledCode, idx: usize) {
        if self.locals[idx].is_container_ref() {
            return;
        }
        let Some(name) = code.locals.get(idx).cloned() else {
            return;
        };
        if let Some(cell_val) = self.read_self_attr_cell(&name) {
            self.locals[idx] = cell_val;
        }
    }

    /// Name-based wrapper: refresh the slot named `name` from `self`'s cell before
    /// a read-modify-write (used by the increment/decrement ops, which dispatch
    /// by name rather than slot index).
    pub(super) fn sync_attr_local_from_cell_by_name(&mut self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        if let Some(slot) = self.find_local_slot(code, name) {
            self.sync_attr_local_from_cell(code, slot);
        }
    }

    /// Name-based wrapper: mirror the slot named `name` into `self`'s cell after a
    /// read-modify-write.
    pub(super) fn mirror_attr_local_to_cell_by_name(&self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        if let Some(slot) = self.find_local_slot(code, name) {
            self.mirror_attr_local_to_cell(code, slot);
        }
    }
}
