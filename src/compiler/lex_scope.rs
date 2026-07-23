//! The lexical-scope questions `OUTER::` / `OUTERS::` ask, in one place.
//!
//! `OUTER::` is a *lexical* construct, so which binding it names is settled by
//! the shape of the source, not by anything the VM can observe while running:
//! the compiler owns the answer (see [`Compiler::emit_outer_var_access`]). That
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

/// One compiled scope's declarations: name -> the slot recorded for it (`None`
/// unless a shadow-slot build recorded one). Mirrors `Compiler::local_scopes`.
pub(crate) type ScopeFrame = HashMap<String, Option<u32>>;

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
}

impl LexScopeChain {
    pub(crate) fn new(
        scopes: Vec<ScopeFrame>,
        local_map: HashMap<String, u32>,
        unit_root_index: usize,
    ) -> Self {
        Self {
            scopes,
            local_map,
            unit_root_index,
        }
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
        self.scopes
            .iter()
            .flat_map(|f| f.keys().map(String::as_str))
    }
}
