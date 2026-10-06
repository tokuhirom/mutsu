//! The lexical-scope questions `OUTER::` / `OUTERS::` ask, in one place.
//!
//! `OUTER::` is a *lexical* construct, so which binding it names is settled by
//! the shape of the source, not by anything the VM can observe while running:
//! the compiler owns the answer (see [`Compiler::emit_outer_var_access`](crate::compiler::Compiler::emit_outer_var_access)). That
//! works only as long as the pseudo-package is spelled literally. The indirect
//! form `$::($name)::x` computes the very same lookup from a string that does
//! not exist until run time, so the *decision* must be reachable from the VM
//! while the *inputs* to it stay compile-time.
//!
//! This module is the seam: [`resolve_outer`] / [`resolve_outers`] take the
//! scope chain as plain data, so the compiler calls them with its live fields
//! and the VM calls them through a [`LexScopeChain`] baked into the code chunk
//! at the emit point. Both hand over the same chain (`Compiler::full_scope_chain`),
//! so one implementation answers both spellings and they cannot drift.

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/// One compiled scope's declarations: name -> the slot recorded for it (`None`
/// unless a shadow-slot build recorded one). Mirrors `Compiler::local_scopes`.
///
/// A frame dereferences to that name -> slot map, and also remembers the
/// declared type of each plain scalar the scope declares
/// ([`ScopeFrame::record_scalar_type`]). Keeping the type in the frame, rather
/// than in a table parallel to the chain, is what keeps it in step with the
/// scope: the frame is popped, parked (`pending_scope_frame`) and handed to a
/// nested compiler as one value, so a type can never outlive, or be attributed
/// to, a scope it was not declared in.
#[derive(Debug, Clone, Default)]
pub(crate) struct ScopeFrame {
    slots: HashMap<String, Option<u32>>,
    /// The declared type of each plain `my`/`state`/`our` scalar this scope
    /// declares (`None` when it is untyped). Compile-time only: it is not part
    /// of a baked [`LexScopeChain`]'s encoding.
    scalar_types: HashMap<String, Option<String>>,
}

impl ScopeFrame {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Record that this scope declares the plain scalar `name` with the declared
    /// type `ty` (`None` for an untyped one).
    // Cost: O(|name| + |ty|).
    pub(crate) fn record_scalar_type(&mut self, name: &str, ty: Option<&str>) {
        self.scalar_types
            .insert(name.to_string(), ty.map(str::to_string));
    }

    /// The declared type recorded for the scalar `name`: `None` when this scope
    /// recorded nothing for it, `Some(None)` for an untyped declaration.
    // Cost: O(|name|).
    pub(crate) fn scalar_type(&self, name: &str) -> Option<Option<&str>> {
        self.scalar_types.get(name).map(Option::as_deref)
    }
}

impl Deref for ScopeFrame {
    type Target = HashMap<String, Option<u32>>;
    fn deref(&self) -> &Self::Target {
        &self.slots
    }
}

impl DerefMut for ScopeFrame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.slots
    }
}

impl FromIterator<(String, Option<u32>)> for ScopeFrame {
    fn from_iter<I: IntoIterator<Item = (String, Option<u32>)>>(iter: I) -> Self {
        Self {
            slots: iter.into_iter().collect(),
            scalar_types: HashMap::new(),
        }
    }
}

impl<'a> IntoIterator for &'a ScopeFrame {
    type Item = (&'a String, &'a Option<u32>);
    type IntoIter = std::collections::hash_map::Iter<'a, String, Option<u32>>;
    fn into_iter(self) -> Self::IntoIter {
        self.slots.iter()
    }
}

impl IntoIterator for ScopeFrame {
    type Item = (String, Option<u32>);
    type IntoIter = std::collections::hash_map::IntoIter<String, Option<u32>>;
    fn into_iter(self) -> Self::IntoIter {
        self.slots.into_iter()
    }
}

/// What an `OUTER::` / `OUTERS::` lookup of one name resolves to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OuterResolution {
    /// The named scope does not declare the name, so the lookup is Nil.
    /// `OUTER` names exactly ONE scope (packages.rakudoc: "Symbols in the next
    /// outer lexical scope"), so it must NOT fall back to whatever an enclosing
    /// scope happens to bind under the same name -- that cascade is `OUTERS`.
    NotDeclared,
    /// Read the name from `depth` scopes out. `slot` is the emit-point slot of
    /// the binding visible there; only shadow-slot builds consult it.
    Read { depth: usize, slot: Option<u32> },
}

/// The slot of the binding `bare` has `depth` scopes out, as seen from the
/// emit point: start at the currently-visible slot and undo each intervening
/// scope's shadowing record, innermost first.
fn resolve_slot(
    scopes: &[ScopeFrame],
    local_map: &HashMap<String, u32>,
    name: &str,
    depth: usize,
) -> Option<u32> {
    let n = scopes.len();
    if depth >= n {
        return None;
    }
    let target = n - 1 - depth;
    let mut slot = local_map.get(name).copied();
    for frame in scopes[target + 1..].iter().rev() {
        if let Some(prev) = frame.get(name) {
            slot = *prev;
        }
    }
    slot
}

/// Resolve `OUTER::` at `depth` (`$OUTER::x` is depth 1, `$OUTER::OUTER::x` is 2).
///
/// Whenever the target scope is inside this compilation frame the answer is
/// settled outright -- `scopes` records which names each scope declares, so
/// "declared there?" needs no runtime guessing. Only a `depth` that crosses the
/// outermost frame is unknowable here (the enclosing frame is a separate
/// compilation), and is left to the runtime's captured-env walk in
/// `Interpreter::get_outer_var`.
pub(crate) fn resolve_outer(
    scopes: &[ScopeFrame],
    local_map: &HashMap<String, u32>,
    bare: &str,
    depth: usize,
) -> OuterResolution {
    let n = scopes.len();
    // The topic is exempt: EVERY block and routine scope declares its own `$_`
    // (a block's implicitly as `$_ is raw = OUTER::<$_>`, a routine's as a fresh
    // undefined one), so "does the target scope declare it?" is always yes and
    // the question is only ever what that `$_` holds -- which the runtime path
    // answers. `_` is never in `scopes` because it is never spelled as a
    // declaration, so without this it would resolve to a constant Nil.
    let implicitly_declared = bare == "_";
    if depth < n && !implicitly_declared && !scopes[n - 1 - depth].contains_key(bare) {
        return OuterResolution::NotDeclared;
    }
    OuterResolution::Read {
        depth,
        slot: resolve_slot(scopes, local_map, bare, depth),
    }
}

/// Whether `OUTER::` at `depth` names the very binding an unqualified lookup of
/// `bare` sees from the emit point -- i.e. no scope between the current one
/// (inclusive) and the target scope (exclusive) shadows it.
///
/// A WRITE through `$OUTER::x` (`$OUTER::x := $y`, `$OUTER::x = 5`) is then
/// exactly a write to the plain `$x`, so the compiler can route it through the
/// ordinary assignment / rebind machinery, which already keeps the declaring
/// slot, a captured cell and the env overlay coherent (ADR-0097 §15).
///
/// The target scope must declare `bare` when it lies inside the chain. A
/// `depth` that crosses the chain's outermost scope is answered the way the
/// read side ([`resolve_outer`]) answers it -- by the runtime's outward
/// cascade -- so it qualifies whenever no in-chain scope shadows the name.
/// The topic `$_` never qualifies: every block declares its own.
// Cost: O(d), d = the number of scopes between the emit point and the target.
pub(crate) fn outer_is_visible_binding(scopes: &[ScopeFrame], bare: &str, depth: usize) -> bool {
    if depth == 0 || bare == "_" {
        return false;
    }
    let n = scopes.len();
    let inner = if depth < n {
        if !scopes[n - 1 - depth].contains_key(bare) {
            return false;
        }
        &scopes[n - depth..]
    } else {
        scopes
    };
    !inner.iter().any(|frame| frame.contains_key(bare))
}

/// The index, in `scopes`, of the scope `OUTER::` at `depth` names, when that
/// scope lies inside the chain and declares `bare` -- i.e. when the lookup
/// names one definite binding (#10827). `None` for the topic, for a depth that
/// crosses the chain's outermost scope, and for a scope that does not declare
/// the name (the read side answers that one with a constant Nil).
// Cost: O(1).
pub(crate) fn outer_target_index(scopes: &[ScopeFrame], bare: &str, depth: usize) -> Option<usize> {
    let n = scopes.len();
    if depth == 0 || depth >= n || bare == "_" {
        return None;
    }
    let target = n - 1 - depth;
    scopes[target].contains_key(bare).then_some(target)
}

/// The emit-point slot of the binding `bare` has in the scope at index
/// `target` of `scopes` (see [`outer_target_index`]). `None` unless a
/// shadow-slot build recorded one for every intervening shadow.
// Cost: O(d), d = the number of scopes between the emit point and the target.
pub(crate) fn slot_at_index(
    scopes: &[ScopeFrame],
    local_map: &HashMap<String, u32>,
    bare: &str,
    target: usize,
) -> Option<u32> {
    let n = scopes.len();
    if target >= n {
        return None;
    }
    resolve_slot(scopes, local_map, bare, n - 1 - target)
}

/// Resolve `OUTERS::` -- packages.rakudoc: "Symbols in any outer lexical scope".
///
/// Where `OUTER` names one scope, `OUTERS` searches outward and stops at the
/// innermost ENCLOSING scope that declares the name; the current scope is
/// excluded, so `my $y = 7; { my $y = 8; say $OUTERS::y }` is 7, not 8. That
/// makes it exactly an `OUTER` at the depth of the first enclosing declaration,
/// so the two share one implementation and only the choice of depth differs.
pub(crate) fn resolve_outers(
    scopes: &[ScopeFrame],
    local_map: &HashMap<String, u32>,
    bare: &str,
) -> OuterResolution {
    let n = scopes.len();
    for depth in 1..n {
        if scopes[n - 1 - depth].contains_key(bare) {
            return resolve_outer(scopes, local_map, bare, depth);
        }
    }
    // No enclosing scope of THIS frame declares it. The search continues in the
    // enclosing frame, which is a separate compilation: depth `n` crosses this
    // frame's boundary, which is precisely the case `get_outer_var` resolves
    // against the captured env -- itself an outward cascade, i.e. OUTERS.
    resolve_outer(scopes, local_map, bare, n)
}

/// Resolve `UNIT::` — the OUTERMOST lexical scope of the compilation unit
/// (`$UNIT::x`, `UNIT::<$x>`). `OUTER::` names one scope out; `UNIT::` names the
/// top of the chain: `my $x=1; { my $x=2; { my $x=3; $UNIT::x } }` is 1. That is
/// exactly an `OUTER::` at the maximum in-frame depth (`n - 1`), so the two share
/// one implementation. A `UNIT::` reached from inside a routine (a separate
/// compilation frame) still names the file mainline: the routine's compiler
/// inherits the enclosing scopes into its chain, so the outermost frame here IS
/// the mainline; only when that chain is itself severed does the runtime env walk
/// (depth `n`) take over.
pub(crate) fn resolve_unit(
    scopes: &[ScopeFrame],
    local_map: &HashMap<String, u32>,
    bare: &str,
    unit_root_index: usize,
) -> OuterResolution {
    let n = scopes.len();
    if n == 0 {
        return OuterResolution::Read {
            depth: 0,
            slot: local_map.get(bare).copied(),
        };
    }
    // The unit's outermost scope sits at `unit_root_index` from the front; the
    // depth that reaches it from the innermost (current) scope is its distance
    // from the end. Guarded so a stale index can never wrap below zero.
    let depth = (n - 1).saturating_sub(unit_root_index);
    resolve_outer(scopes, local_map, bare, depth)
}

/// The compile-time scope chain at one emit point, baked into the code chunk.
///
/// A literal `$OUTER::x` never needs this: the compiler resolves it and emits
/// the answer. It exists for `$::($name)::x`, where the pseudo-package is only
/// known at run time and the VM must therefore ask the question itself -- with
/// the compiler's inputs, not the runtime scope stack, which is dynamic rather
/// than lexical and would give a different (wrong) answer inside a closure.
#[derive(Debug, Clone)]
pub(crate) struct LexScopeChain {
    scopes: Vec<ScopeFrame>,
    local_map: HashMap<String, u32>,
    /// Index of the compilation unit's outermost scope (for `UNIT::`), baked in
    /// so the runtime `$::('UNIT')::x` walk stops at the same boundary the
    /// compile-time `$UNIT::x` does — notably one frame past an `EVAL` wrapper.
    unit_root_index: usize,
    /// True when this deref site sits inside an *immediate* block (bare block /
    /// `if` / `for` / `while` body). There an indirect `$::('CALLER')::x` /
    /// `$::('CALLERS')::x` names the lexical parent chain, not the runtime call
    /// stack (which the block never pushed a frame onto) — the same routing the
    /// literal `$CALLER::x` / `$CALLERS::x` spellings get in the compiler.
    caller_lexical: bool,
}

impl LexScopeChain {
    pub(crate) fn new(
        scopes: Vec<ScopeFrame>,
        local_map: HashMap<String, u32>,
        unit_root_index: usize,
        caller_lexical: bool,
    ) -> Self {
        Self {
            scopes,
            local_map,
            unit_root_index,
            caller_lexical,
        }
    }

    /// Whether an indirect `CALLER::` / `CALLERS::` at this site resolves
    /// lexically (immediate-block context — see the `caller_lexical` field).
    pub(crate) fn caller_is_lexical(&self) -> bool {
        self.caller_lexical
    }

    pub(crate) fn resolve_outer(&self, bare: &str, depth: usize) -> OuterResolution {
        resolve_outer(&self.scopes, &self.local_map, bare, depth)
    }

    pub(crate) fn resolve_outers(&self, bare: &str) -> OuterResolution {
        resolve_outers(&self.scopes, &self.local_map, bare)
    }

    pub(crate) fn resolve_unit(&self, bare: &str) -> OuterResolution {
        resolve_unit(&self.scopes, &self.local_map, bare, self.unit_root_index)
    }

    /// Every name any scope in the chain declares — i.e. every name a lookup
    /// through this chain could possibly land on.
    ///
    /// A literal `$OUTER::x` tells `CompiledCode::compute_free_vars` exactly which
    /// enclosing binding a closure must snapshot under `__mutsu_outer::`. An
    /// indirect `$::($name)::x` names it only at run time, far too late to
    /// snapshot anything, so its site has to claim the whole chain instead. The
    /// over-claim is cheap — a symbolic deref forces a whole-env capture anyway —
    /// while an under-claim silently falls back to the dynamic scope stack.
    pub(crate) fn declared_names(&self) -> impl Iterator<Item = &str> {
        // Name order within each frame, not the frame map's hash order: the
        // result is recorded in compiled code (`outer_ref_names`), which must
        // come out the same in every process (ADR-11756 §2.5).
        self.scopes.iter().flat_map(|f| {
            let mut names: Vec<&str> = f.keys().map(String::as_str).collect();
            names.sort_unstable();
            names
        })
    }
}

/// Encoded with every frame and `local_map` in key order, so the encoding of a
/// chain does not depend on the maps' per-instance hash seeds (ADR-11756 §2.4).
impl bincode::Encode for LexScopeChain {
    // Cost: O(n log n), n = locals in the map.
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        let LexScopeChain {
            scopes,
            local_map,
            unit_root_index,
            caller_lexical,
        } = self;
        (scopes.len() as u64).encode(encoder)?;
        for frame in scopes {
            let mut entries: Vec<(&String, &Option<u32>)> = frame.iter().collect();
            entries.sort();
            entries.encode(encoder)?;
        }
        let mut locals: Vec<(&String, &u32)> = local_map.iter().collect();
        locals.sort();
        locals.encode(encoder)?;
        unit_root_index.encode(encoder)?;
        caller_lexical.encode(encoder)
    }
}

impl bincode::Decode<crate::precomp_codec::DecodeCtx> for LexScopeChain {
    // Cost: O(n), n = size of the chain.
    fn decode<D: bincode::de::Decoder<Context = crate::precomp_codec::DecodeCtx>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        let frames = u64::decode(decoder)? as usize;
        let mut scopes = Vec::with_capacity(frames);
        for _ in 0..frames {
            let entries: Vec<(String, Option<u32>)> = bincode::Decode::decode(decoder)?;
            scopes.push(entries.into_iter().collect::<ScopeFrame>());
        }
        let locals: Vec<(String, u32)> = bincode::Decode::decode(decoder)?;
        Ok(LexScopeChain {
            scopes,
            local_map: locals.into_iter().collect(),
            unit_root_index: bincode::Decode::decode(decoder)?,
            caller_lexical: bincode::Decode::decode(decoder)?,
        })
    }
}
bincode::impl_borrow_decode_with_context!(LexScopeChain, crate::precomp_codec::DecodeCtx);
