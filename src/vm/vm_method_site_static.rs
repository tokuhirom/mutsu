//! The method-site lane's static checks, decided once per call site.
//!
//! `site_lane_answer` used to ask on every call a handful of questions whose
//! answers depend only on the call site, never on the values it is called
//! with:
//! - whether any built-in row is named by the method at this arity;
//! - whether the site is a `map`/`grep`/`first` on an `@` variable (left to
//!   the rw-writeback dispatch);
//! - whether the receiver variable is an `@!`/`%!` attribute;
//! - whether every argument source is positional;
//! - for an inline receiver, whether the name is one the full path treats
//!   specially (`perl`, `throw`, ...).
//!
//! Together these cost ~250 instructions per call (a constant-pool string
//! compare per name, an argument-descriptor walk). They are a pure function of
//! the chunk, so they are now computed once per chunk into
//! [`CompiledCode::method_site_static`] and read as one indexed load
//! (ADR-18357 stage 3). The dynamic questions (pending writebacks, a user
//! `find_method`, the receiver's shape, the registry generation, the
//! arguments' types) are still asked per call.

use super::*;
use crate::builtins::method_table;

/// The most arguments a lane-answered call takes (the lane copies them into a
/// buffer this long).
pub(super) const MAX_LANE_ARITY: usize = 2;

/// Names an inline-receiver `CallMethod` always leaves to the full path.
const INLINE_RECEIVER_FULL_PATH: [&str; 5] =
    ["perl", "throw", "of", "package_at_key", "ASSIGN-KEY"];

impl Interpreter {
    /// Whether the static half of the lane admits the method call at
    /// `code.ops[ip]` (see the module doc). `false` for any other op.
    // Cost: O(1) after the first call per chunk (a `OnceLock` acquire and an
    // indexed load); the first call is O(o + a), o = ops, a = argument
    // descriptors' entries.
    #[inline]
    /// Whether `name` is one the site lane leaves to the full path when the
    /// receiver is an inline value rather than a variable
    /// (`INLINE_RECEIVER_FULL_PATH`): the extra refusal a value call at a
    /// `CallMethodMut` site needs on top of that site's static verdict
    /// (`slot_body_value_call`).
    // Cost: O(1).
    pub(super) fn inline_receiver_full_path(name: &str) -> bool {
        INLINE_RECEIVER_FULL_PATH.contains(&name)
    }

    pub(super) fn method_site_static_ok(code: &CompiledCode, ip: usize) -> bool {
        code.method_site_static
            .get_or_init(|| {
                (0..code.ops.len())
                    .map(|i| Self::compute_method_site_static(code, i))
                    .collect()
            })
            .get(ip)
            .copied()
            .unwrap_or(false)
    }

    // Cost: O(a), a = the site's argument descriptor entries.
    fn compute_method_site_static(code: &CompiledCode, ip: usize) -> bool {
        let (name_idx, arity, target_name_idx, arg_sources_idx) = match code.ops[ip] {
            OpCode::CallMethodMut {
                name_idx,
                arity,
                target_name_idx,
                modifier_idx: None,
                quoted: false,
                arg_sources_idx,
                ..
            } => (name_idx, arity, Some(target_name_idx), arg_sources_idx),
            OpCode::CallMethod {
                name_idx,
                arity,
                modifier_idx: None,
                quoted: false,
                arg_sources_idx,
            } => (name_idx, arity, None, arg_sources_idx),
            _ => return false,
        };
        let arity = arity as usize;
        let name = Self::const_str(code, name_idx);
        arity <= MAX_LANE_ARITY
            // Most calls name a method no row has.
            && method_table::names_a_row(code.const_sym(name_idx), arity)
            // A callback that binds `$_` rw writes back into the named array
            // (`@a.map({ $_++ })`): the rw-writeback dispatch must see the
            // call (ADR-11276 §9.60). An inline receiver is not told apart.
            && !(matches!(name, "map" | "grep" | "first")
                && target_name_idx.is_none_or(|t| Self::const_str(code, t).starts_with('@')))
            // A property of the receiver variable, not of the method.
            && !target_name_idx
                .is_some_and(|t| Self::is_array_hash_attr_twigil(Self::const_str(code, t)))
            && (target_name_idx.is_some() || !INLINE_RECEIVER_FULL_PATH.contains(&name))
            && (arity == 0
                || arg_sources_idx.is_none_or(|idx| site_args_are_positional(code, idx)))
    }
}

/// Whether a call site's argument-source descriptor (see
/// `Compiler::add_arg_sources_constant`) names only positional arguments: no
/// named argument (`FALSE`, or an array led by it) and no `|` spread
/// (`TRUE`). A named argument reaches the stack as a `Pair`, which
/// `plain_args` refuses anyway; the spread is refused because it changes the
/// argument count at run time.
// Cost: O(a), a = arguments of the site.
fn site_args_are_positional(code: &CompiledCode, idx: u32) -> bool {
    let ValueView::Array(items, ..) = code.constants[idx as usize].view() else {
        return false;
    };
    items.iter().all(|item| {
        matches!(
            item.view(),
            ValueView::Nil | ValueView::Str(_) | ValueView::Pair(..)
        )
    })
}
