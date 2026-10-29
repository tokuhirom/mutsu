//! A frame root's static link: by-name lookups that skip the caller frames
//! (ADR-12529 phase 3).
//!
//! A frame's env is chained over its caller's (`Env::scoped_child`), so a
//! by-name lookup that misses the frame walks on into the frames that called
//! it. For a name that resolves dynamically (`$*x`, `CALLER::`) that walk is
//! the point; for a lexical it is the bug of the ADR's §1.3: `EVAL q[$x]` or
//! `::('$x')` in a routine finds whatever `$x` its caller happens to hold.
//!
//! A routine declared at the top of a compunit -- outside every routine and
//! closure body -- has the program scope as its lexical outer. When such a
//! routine's body looks names up reflectively (`CompiledCode::
//! needs_reflective_capture`), its frame root carries [`StaticLink::UnitOuter`]:
//! a *plain user lexical* (`Symbol::is_plain_user_lexical`) that misses the
//! frame's own tiers continues at the program scope's tiers, past every caller
//! frame in between, and so does a bare type-shaped name (`SecretType`, which a
//! caller's `my class` binds in its frame). Every other name -- dynamics,
//! `self`, the topic, `__mutsu_*` metadata -- walks the whole chain as before.
//!
//! `EVAL $code, context => $ctx` asks for the scope `$ctx` names instead of
//! the running one. mutsu does not carry a frame's lexical scope in a
//! `PseudoStash` yet, so such an EVAL resolves through the whole chain, as
//! every lookup did before the static link existed ([`suppressed`]).
//!
//! Only the lookups follow the link. A flattened copy of the chain
//! (`Env::flattened` and kin) stays faithful to the whole chain, because it is
//! not only a view: a deep chain is replaced by its flattening, and a frame's
//! saved and restored env is one, so dropping the caller frames' names there
//! would lose their writes (a closure's write to `$z` that its caller has not
//! merged back yet is in exactly such a tier).

use super::Env;
pub(crate) use super::outer_scope::OuterScope;
use crate::symbol::Symbol;
use crate::value::Value;
use std::cell::Cell;
use std::sync::Arc;

/// How a frame root's lookups treat the frames chained below it.
pub(crate) enum StaticLink {
    /// The code's lexical outer is the program scope, whose tiers start at
    /// `target` (the chain below the deepest caller frame). A plain user
    /// lexical that misses the frame continues there. With `capture_first`
    /// -- a closure created inside a call frame -- the frame's own capture
    /// (`Env::fallback`) answers first: it holds the creating frame's scope,
    /// which lies between the closure and the program scope.
    UnitOuter {
        target: Arc<Env>,
        capture_first: bool,
        /// The frame's capture when it is a named sub's declaring activation
        /// ([`Env::mark_activation_capture`]) or a closure's created in such
        /// a frame: it binds names the per-routine stores also key, and wins
        /// over them ([`Env::frame_capture_holds`]). Kept on the link, not
        /// only as the root's fallback, so a flattening of the frame's chain
        /// records it too ([`Env::flattened_recording_program_scope`]).
        activation: Option<Arc<crate::env_capture_view::CaptureView>>,
        /// Without `capture_first`: the names the frame's capture still
        /// answers first -- a program-scope capture's nested-block lexicals
        /// ([`Env::program_scope_block_names`], #13595).
        capture_names: Option<Arc<rustc_hash::FxHashSet<Symbol>>>,
    },
}

thread_local! {
    /// Depth of `EVAL ..., context => ...` calls running on this thread. While
    /// it is non-zero a static link is not followed -- see the module docs.
    static SUPPRESSED: Cell<u32> = const { Cell::new(0) };
}

/// Whether static links are currently ignored on this thread.
#[inline]
pub(crate) fn suppressed() -> bool {
    SUPPRESSED.with(Cell::get) != 0
}

/// Run `f` with static links ignored: an EVAL whose `context` names a scope
/// the running frame cannot express. Restores the previous depth on unwind.
pub(crate) fn with_static_links_suppressed<R>(f: impl FnOnce() -> R) -> R {
    struct Guard;
    impl Drop for Guard {
        fn drop(&mut self) {
            SUPPRESSED.with(|c| c.set(c.get() - 1));
        }
    }
    SUPPRESSED.with(|c| c.set(c.get() + 1));
    let _guard = Guard;
    f()
}

/// Whether two bindings of one name hold the same variable: the same value,
/// or the same container seen through a shared cell (a store into a
/// captured variable seeds the cell's container under the name).
// Cost: O(1).
pub(super) fn same_variable(a: &Value, b: &Value) -> bool {
    // `a` is read out before `b` is touched: nesting the two `with_deref`
    // locks deadlocks (the `Mutex` is not reentrant) when `b`'s cell chain
    // runs through the very cell `a` resolves to.
    a.same_binding(b) || {
        let a = a.deref_container();
        b.with_deref(|b| a.same_binding(b))
    }
}

/// Whether a static link skips the caller frames for `key`: a plain user
/// lexical, an attribute variable (`$!t` belongs to the `self` of the method
/// that encloses the code, never to a caller's invocant, #12478), or a bare
/// type-shaped name (an unqualified identifier starting with an uppercase
/// letter). A caller frame binds the latter only for a type it declared
/// lexically (`my class`); a package-scoped type is found through the symbol
/// table, not the frame. A plain lexical's own record (`__mutsu_type::x`,
/// its source-name and hash-key records) follows the variable (ADR-13757
/// step 4): a caller's `my Int $x` says nothing about the running code's `$x`.
/// A block-level module merge (`MetaNs::ModuleMerge`) is the block's lexical
/// import of the module's packages: a caller's `{ use M; ... }` does not
/// grant `M::Foo` to the code it calls (#13801).
// Cost: O(1) for a plain user lexical, an attribute or a memoized record key
// (memoized flags and subject); O(|key|) otherwise.
pub(super) fn lexically_scoped(key: Symbol) -> bool {
    key.is_plain_user_lexical()
        || key.is_attr_twigil_env_key()
        || key
            .shadow_meta_subject()
            .is_some_and(Symbol::is_plain_user_lexical)
        || key.with_str(|s| {
            s.as_bytes().first().is_some_and(u8::is_ascii_uppercase)
                && s.bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
        })
        || key.with_str(|s| s.starts_with(crate::meta_ns::MetaNs::ModuleMerge.prefix()))
}

/// Whether a frame running a loaded module's routine
/// ([`Env::mark_outer_is_foreign_unit`]) stops a lookup of `key` that missed
/// it: a [`lexically_scoped`] name, or a sigilless term (`\x`, an imported
/// or declared `constant`/`my \x`), none of which the program scope holds
/// for that routine.
// Cost: O(1) (memoized symbol flags), O(|key|) for a term probe.
fn foreign_unit_hides(key: Symbol) -> bool {
    lexically_scoped(key) || key.with_str(|s| s.starts_with(crate::term_names::TERM_PREFIX))
}

impl Env {
    /// Give this frame root a static link to the program scope: by-name
    /// lookups of plain user lexicals that miss the frame skip the caller
    /// frames below it. The program scope is the part of the chain below the
    /// deepest frame root -- or, when a deeper root already links there, that
    /// root's target.
    // Cost: O(d), d = tiers of the caller chain walked to its deepest frame
    // root (bounded by MAX_OVERLAY_DEPTH); O(1) when the nearest caller frame
    // is linked already.
    pub(crate) fn link_static_outer_to_unit(
        &mut self,
        capture_first: bool,
        activation: bool,
        capture_names: Option<Arc<rustc_hash::FxHashSet<Symbol>>>,
    ) {
        let activation = if activation {
            self.fallback.clone()
        } else {
            None
        };
        debug_assert!(self.frame_root, "only a frame root has a static link");
        let Some(parent) = &self.parent else {
            return;
        };
        let mut seg: &Arc<Env> = parent;
        let mut cur: &Arc<Env> = parent;
        loop {
            if cur.frame_root {
                if let Some(link) = cur.inner.static_link() {
                    let StaticLink::UnitOuter { target, .. } = link;
                    seg = target;
                    break;
                }
                if let Some(below) = &cur.parent {
                    seg = below;
                }
            }
            match &cur.parent {
                Some(below) => cur = below,
                None => {
                    // A flattened frame below: its record names the program
                    // scope (see `flattened_program_scope`).
                    if let Some(StaticLink::UnitOuter { target, .. }) = cur.inner.static_link() {
                        seg = target;
                    }
                    break;
                }
            }
        }
        let seg = Arc::clone(seg);
        self.cow_mut()
            .set_static_link(Arc::new(StaticLink::UnitOuter {
                target: seg,
                capture_first,
                activation,
                capture_names,
            }));
        self.chain_has_static_link = true;
    }

    /// Whether any tier of this chain is a call frame's root -- false for an
    /// env running the program scope itself (the mainline and its blocks).
    // Cost: O(d), d = chain tiers.
    pub(crate) fn chain_has_frame_root(&self) -> bool {
        let mut cur = self;
        loop {
            if cur.frame_root {
                return true;
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                // A flattened call frame still ran in a frame (see
                // `flattened_program_scope`).
                None => return cur.inner.static_link().is_some(),
            }
        }
    }

    /// [`Self::flattened`] for a flatten that replaces the chain a live frame
    /// runs in (`flatten_scoped_env`, `MAX_OVERLAY_DEPTH`): the collapsed tier
    /// records the program scope below the chain's call frames
    /// ([`Self::flattened_program_scope`]). A capture's flatten does not: a
    /// closure's frame reaches its capture through a fallback, never as the
    /// bottom of its chain.
    // Cost: O(n) for the flatten, n = visible entries; plus O(d), d = chain
    // tiers.
    pub(crate) fn flattened_recording_program_scope(&self) -> Env {
        let mut flat = self.flattened();
        if let Some(record) = self.flattened_program_scope() {
            let StaticLink::UnitOuter { target, .. } = &*record;
            // A chain whose frames hold nothing collapses to the program
            // scope itself, which needs no record of itself.
            if !Arc::ptr_eq(&flat.inner, &target.inner) {
                // An activation's capture stays observable to the per-name
                // store checks (`frame_capture_holds`), which the chain flag
                // lets in.
                if matches!(
                    &*record,
                    StaticLink::UnitOuter {
                        activation: Some(_),
                        ..
                    }
                ) {
                    flat.chain_has_static_link = true;
                }
                flat.cow_mut().set_static_link(record);
            }
        }
        flat
    }

    /// What a flattening of this chain keeps of its frame structure: the
    /// program scope below its call frames, which
    /// [`Self::flattened_recording_program_scope`] records on the collapsed
    /// tier as a [`StaticLink`] of a non-root env. `None` when the chain is
    /// the program scope itself.
    ///
    /// A flattened env merges a frame's names with every caller's and the
    /// program scope's, so "below the deepest frame root" no longer finds the
    /// program scope once a frame is collapsed into the bottom of a chain: a
    /// frame linked above it would skip its callers straight into theirs
    /// (#12478). The record is inert for lookups through the flattened env
    /// itself, which only a frame root's link redirects; it is read where the
    /// program scope is looked for (`program_scope_from`,
    /// `link_static_outer_to_unit`, the next flattening).
    // Cost: O(d), d = chain tiers.
    fn flattened_program_scope(&self) -> Option<Arc<StaticLink>> {
        // The running frame's declaring activation, if any, survives the
        // flattening with the record.
        let activation = self.frame_activation().cloned();
        let unit = |target: &Arc<Env>| {
            Arc::new(StaticLink::UnitOuter {
                target: Arc::clone(target),
                capture_first: false,
                activation: activation.clone(),
                capture_names: None,
            })
        };
        let mut seg: Option<&Arc<Env>> = None;
        let mut cur = self;
        loop {
            if cur.frame_root {
                if let Some(StaticLink::UnitOuter { target, .. }) = cur.inner.static_link() {
                    return Some(unit(target));
                }
                if let Some(below) = &cur.parent {
                    seg = Some(below);
                }
            }
            match &cur.parent {
                Some(below) => cur = below,
                None => break,
            }
        }
        if let Some(StaticLink::UnitOuter { target, .. }) = cur.inner.static_link() {
            return Some(unit(target));
        }
        seg.map(unit)
    }

    /// [`Self::flattened`] as the scope this env's static links describe: a
    /// plain user lexical that only a skipped caller frame binds is dropped,
    /// and one a skipped frame shadows takes the value the link's target
    /// gives it. A *view*, for a reflective closure's capture -- never a
    /// replacement for the chain (see the module docs).
    // Cost: O(n) for the flatten, n = visible entries; plus O(h * d) when the
    // chain holds a static link, h = plain user lexicals the skipped frames
    // bind, d = chain tiers.
    pub(crate) fn flattened_static_view(&self) -> Env {
        let mut flat = self.flattened();
        if !self.chain_has_static_link || suppressed() {
            return flat;
        }
        let mut hidden: Vec<Symbol> = Vec::new();
        let mut target: Option<*const Env> = None;
        // Only the first frame root's link applies, as in the lookup walk.
        let mut origin_root_seen = false;
        let mut cur = self;
        loop {
            if target.is_some_and(|t| std::ptr::eq(t, cur)) {
                target = None;
            }
            if target.is_some() {
                hidden.extend(cur.inner.keys().copied().filter(|k| lexically_scoped(*k)));
                if let Some(fb) = &cur.fallback {
                    hidden.extend(fb.iter().map(|(k, _)| *k).filter(|k| lexically_scoped(*k)));
                }
            } else if cur.frame_root && origin_root_seen {
                // A caller frame's link: not this scope's.
            } else if cur.outer_is_unit
                && let Some(parent) = cur.parent.as_deref()
            {
                target = Some(std::ptr::from_ref(parent.program_scope_from()));
                if let Some(names) = cur.inner.unit_hidden() {
                    hidden.extend(names.iter().copied());
                }
            } else if let Some(link) = cur.inner.static_link() {
                let StaticLink::UnitOuter { target: seg, .. } = link;
                target = Some(Arc::as_ptr(seg));
            }
            if cur.frame_root {
                origin_root_seen = true;
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        for key in hidden {
            match self.get_sym(key) {
                Some(v) => {
                    let v = v.clone();
                    flat.insert_sym(key, v);
                }
                None => {
                    flat.remove_sym(key);
                }
            }
        }
        flat
    }

    /// The scopes lexically enclosing the frame this env runs in, innermost
    /// first: a closure's capture (the creating scope) when its static link
    /// puts it ahead, then the program scope. Empty at the program scope
    /// itself. Captured when `nqp::ctx` runs, because a context is a
    /// flattened snapshot with no scope structure of its own.
    // Cost: O(d), d = chain tiers walked to the frame root.
    pub(crate) fn enclosing_scopes(&self) -> Vec<OuterScope> {
        let mut root = self;
        while !root.frame_root {
            match root.parent.as_deref() {
                Some(parent) => root = parent,
                None => return Vec::new(),
            }
        }
        let mut scopes = Vec::new();
        if root.outer_is_unit {
            if let Some(parent) = root.parent.as_deref() {
                scopes.push(OuterScope::Env(parent.program_scope_from().clone()));
            }
        } else if let Some(StaticLink::UnitOuter {
            target,
            capture_first,
            ..
        }) = root.inner.static_link()
        {
            if *capture_first && let Some(fb) = &root.fallback {
                scopes.push(OuterScope::Capture(Arc::clone(fb)));
            }
            scopes.push(OuterScope::Env(Env::clone(target)));
        } else if let Some(fb) = &root.fallback {
            scopes.push(OuterScope::Capture(Arc::clone(fb)));
        }
        scopes
    }

    /// Mark this env, a reflective closure's capture, as taken at program
    /// scope: the closure's frame then links to the program scope
    /// ([`Self::link_static_outer_to_unit`]).
    // Cost: O(1), plus the overlay's copy-on-write when it is shared.
    pub(crate) fn mark_program_scope_capture(&mut self) {
        self.cow_mut().mark_program_scope_capture();
    }

    /// Record on this program-scope capture the nested-block lexicals its
    /// closure's frame answers from the capture first (#13595).
    // Cost: O(1), plus the overlay's copy-on-write when it is shared.
    pub(crate) fn set_program_scope_block_names(&mut self, names: rustc_hash::FxHashSet<Symbol>) {
        if !names.is_empty() {
            self.cow_mut()
                .set_program_scope_block_names(Arc::new(names));
        }
    }

    /// See [`Self::set_program_scope_block_names`].
    // Cost: O(1).
    pub(crate) fn program_scope_block_names(&self) -> Option<Arc<rustc_hash::FxHashSet<Symbol>>> {
        self.inner.program_scope_block_names().cloned()
    }

    /// The plain user lexicals this env binds above the program scope's own
    /// tier: those of the nested blocks running at program scope, which a
    /// closure created here closes over (#13595). Empty in a call frame's
    /// chain, which [`Self::chain_has_frame_root`] answers instead.
    // Cost: O(n), n = entries of the tiers above the chain's tail.
    pub(crate) fn block_lexicals_above_program_scope(&self) -> rustc_hash::FxHashSet<Symbol> {
        let mut names = rustc_hash::FxHashSet::default();
        let mut cur = self;
        while let Some(parent) = cur.parent.as_deref() {
            names.extend(
                cur.inner
                    .keys()
                    .filter(|k| k.is_plain_user_lexical())
                    .copied(),
            );
            cur = parent;
        }
        names
    }

    /// Whether this env is a capture marked by
    /// [`Self::mark_program_scope_capture`].
    // Cost: O(1).
    pub(crate) fn is_program_scope_capture(&self) -> bool {
        self.inner.is_program_scope_capture()
    }

    /// [`Self::get`] through the whole chain, callers included, whatever
    /// static links it holds. For the writeback machinery that carries a
    /// value *across* frames (a runtime-named write a callee frame holds for
    /// its caller): it reads where a value currently is, not what a name
    /// means in this frame's scope.
    // Cost: O(d), d = chain tiers.
    pub(crate) fn get_through_callers(&self, key: &str) -> Option<&Value> {
        with_static_links_suppressed(|| self.get(key))
    }

    /// [`Self::contains_key_sym`] through the whole chain, callers included,
    /// whatever static links it holds -- the membership twin of
    /// [`Self::get_through_callers`], for the same writeback machinery.
    // Cost: O(d), d = chain tiers.
    pub(crate) fn contains_key_sym_through_callers(&self, key: Symbol) -> bool {
        with_static_links_suppressed(|| self.contains_key_sym(key))
    }

    /// Where a lookup of `key` that missed this env continues, when this env
    /// is a frame root whose static link skips the callers for it.
    // Cost: O(1) for a stored link; O(d) for a flagged frame
    // ([`Self::mark_outer_is_unit`]), d = caller-chain tiers.
    #[inline]
    pub(super) fn static_skip(&self, key: Symbol) -> Option<&Env> {
        if self.outer_is_unit {
            if !lexically_scoped(key) || suppressed() {
                return None;
            }
            return self.parent.as_deref().map(Env::program_scope_from);
        }
        let link = self.inner.static_link()?;
        if !lexically_scoped(key) || suppressed() {
            return None;
        }
        let StaticLink::UnitOuter { target, .. } = link;
        Some(target)
    }

    /// Mark this frame root as running code declared at program scope: a
    /// lexical that misses the frame continues at the program scope, past the
    /// caller frames. The cheap form of [`Self::link_static_outer_to_unit`]
    /// (no allocation, the program scope found when a lookup needs it), for
    /// the routine calls that make up nearly every frame.
    // Cost: O(1).
    #[inline]
    pub(crate) fn mark_outer_is_unit(&mut self) {
        debug_assert!(self.frame_root, "only a frame root has a static link");
        self.outer_is_unit = true;
        self.chain_has_static_link = true;
    }

    /// [`Self::mark_outer_is_unit`] for code declared at the top level of a
    /// compunit that is not the program's (a loaded module's routine). The
    /// program scope past the caller frames is not that code's lexical outer
    /// either: its own file scope lives in the per-compunit stores
    /// (`unit_lexicals`, `module_scope_lexicals`), so a lexically scoped name
    /// that misses the frame stops here ([`Self::static_hides`]). Without it
    /// a module routine's `EVAL '$x'` or `::('Foo')` read the importing
    /// program's own lexicals and the names its top-level `use`s imported.
    // Cost: O(1).
    #[inline]
    pub(crate) fn mark_outer_is_foreign_unit(&mut self) {
        self.mark_outer_is_unit();
        self.outer_is_foreign_unit = true;
    }

    /// Hide `hidden` from this marked frame root's view of the program scope
    /// ([`Self::mark_outer_is_unit`]): the plain user lexicals the program
    /// scope binds that the running code's lexical outer does not declare --
    /// a mainline block's `my` that does not enclose the routine, or a
    /// variable a block-scoped `use` imported (#13604). `None` clears it.
    // Cost: O(1), plus the overlay's copy-on-write when it is shared.
    pub(crate) fn set_unit_hidden(&mut self, hidden: Option<Arc<rustc_hash::FxHashSet<Symbol>>>) {
        debug_assert!(self.frame_root, "only a frame root has a static link");
        if hidden.is_none() && self.inner.unit_hidden().is_none() {
            return;
        }
        self.cow_mut().set_unit_hidden(hidden);
    }

    /// Whether a lookup of `key` that missed this frame root stops here
    /// instead of continuing at the program scope: the root is marked
    /// [`Self::mark_outer_is_unit`] and hides `key` ([`Self::set_unit_hidden`]).
    // Cost: O(1).
    #[inline]
    pub(super) fn static_hides(&self, key: Symbol) -> bool {
        self.outer_is_unit
            && !suppressed()
            && ((self.outer_is_foreign_unit && foreign_unit_hides(key))
                || self
                    .inner
                    .unit_hidden()
                    .is_some_and(|hidden| hidden.contains(&key)))
    }

    /// Whether the frame running this env hides `key` from its view of the
    /// program scope ([`Self::static_hides`] on the nearest frame root): a
    /// name a block-scoped `use` imported, read by a top-level routine that
    /// block calls. A store that answers such a name outside the env (the
    /// importer package's short type aliases) asks this first.
    // Cost: O(d), d = envs between this one and its frame root.
    pub(crate) fn frame_hides_unit_name(&self, key: Symbol) -> bool {
        let mut cur = self;
        loop {
            if cur.frame_root {
                return cur.static_hides(key);
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => return false,
            }
        }
    }

    /// Whether this env is a call frame's root (`Env::scoped_child`), which
    /// can carry a static link.
    // Cost: O(1).
    #[inline]
    pub(crate) fn is_frame_root(&self) -> bool {
        self.frame_root
    }

    /// Undo [`Self::mark_outer_is_unit`] on this frame root, once the code
    /// it ran is done and the call's return path reads values across frames.
    /// The chain-level latch stays set; it only selects the walk that checks.
    // Cost: O(1).
    #[inline]
    pub(crate) fn unmark_outer_is_unit(&mut self) {
        self.outer_is_unit = false;
        self.outer_is_foreign_unit = false;
    }

    /// Whether this env is a frame root marked by [`Self::mark_outer_is_unit`]
    /// with nothing bound in its own overlay -- a frame a callee that is also
    /// declared at program scope can run in without a root of its own, since
    /// both see exactly the program scope past it.
    // Cost: O(1).
    #[inline]
    pub(crate) fn is_empty_unit_frame(&self) -> bool {
        self.outer_is_unit && self.overlay_is_shared_empty()
    }

    /// Whether this frame root is marked [`Self::mark_outer_is_foreign_unit`].
    // Cost: O(1).
    #[inline]
    pub(crate) fn outer_is_foreign_unit(&self) -> bool {
        self.outer_is_foreign_unit
    }

    /// The program scope's part of the chain starting at `self`: the env
    /// below the deepest frame root, or `self` when no frame root is below --
    /// unless the chain bottoms out at a flattened frame, whose record names
    /// it.
    // Cost: O(d), d = chain tiers.
    pub(super) fn program_scope_from(&self) -> &Env {
        let mut seg = self;
        let mut cur = self;
        loop {
            if cur.frame_root
                && let Some(below) = cur.parent.as_deref()
            {
                seg = below;
            }
            match cur.parent.as_deref() {
                Some(below) => cur = below,
                // A flattened frame at the bottom records the program scope
                // its frames ran over (`flattened_program_scope`).
                None => {
                    return match cur.inner.static_link() {
                        Some(StaticLink::UnitOuter { target, .. }) => target,
                        None => seg,
                    };
                }
            }
        }
    }

    /// The value this frame root's own capture gives `key`, when its static
    /// link puts the capture ahead of the program scope.
    // Cost: O(l), l = the capture's layers.
    #[inline]
    pub(super) fn static_capture_hit(&self, key: Symbol) -> Option<&Value> {
        let StaticLink::UnitOuter {
            capture_first,
            capture_names,
            ..
        } = self.inner.static_link()?;
        if !*capture_first
            && !capture_names
                .as_ref()
                .is_some_and(|names| names.contains(&key))
        {
            return None;
        }
        if !lexically_scoped(key) || suppressed() {
            return None;
        }
        self.fallback.as_deref()?.get(&key)
    }
}
