//! The fast binders' named-parameter arm: one implementation shared by the
//! general fast binder (`call_compiled_method_fast_in_unit`), the lean frame
//! (`vm_method_lean_frame`) and the bind-only construction phase step
//! (`runtime::ctor_phase_bind_only`).
//!
//! A named parameter the fast-path gate admitted (`named_param_fast_eligible`:
//! a plain readonly scalar `:$x`, a `:$x is raw` the container classification
//! admitted, or an attributive `:$!x` / `:@!a` / `:%!h`) takes the last
//! argument Pair with its key, shaped as the slow binder's named arm shapes
//! it; an unsupplied one takes its constant fill or its type-object default.
//! An attributive parameter then writes the one attribute through `self`'s
//! shared cell, as `bind_param_value` does, before the frame's locals are
//! seeded from the cell.

use super::*;

/// What a caller of [`Interpreter::bind_named_param_fast`] has to do besides
/// installing the returned value.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum NamedParamBind {
    /// Nothing more.
    Plain,
    /// A `:$x is raw` bound to a plain value: the parameter is a read-only
    /// term once the frame's readonly marks are applied.
    RawReadonly,
    /// An unsupplied parameter whose non-constant default the default
    /// prologue evaluates in the finished frame
    /// (`vm_method_default_prologue`); the returned value is the placeholder.
    DeferredDefault,
}

impl Interpreter {
    /// Bind the named parameter `pd` (index `idx` of `method_def`, declared by
    /// `owner_sym`) from `args`. `attrs_cell` is the invocant's attribute
    /// cell, written for an attributive parameter.
    // Cost: O(a), a = arguments (the last-wins key scan), plus one intern and
    // up to four attribute-map probes for an attributive parameter.
    pub(crate) fn bind_named_param_fast(
        &mut self,
        owner_sym: Symbol,
        method_def: &crate::runtime::MethodDef,
        idx: usize,
        pd: &crate::ast::ParamDef,
        args: &[Value],
        attrs_cell: Option<&crate::gc::Gc<crate::value::InstanceAttrs>>,
    ) -> (Value, NamedParamBind) {
        let mut outcome = NamedParamBind::Plain;
        let bound = Self::named_param_fast_match_key(pd).and_then(|key| {
            args.iter().rev().find_map(|a| {
                let a = a.unwrap_varref();
                // The tag test spares the positional arguments a full
                // `view()` decode.
                if !a.is_string_pair_value() {
                    return None;
                }
                match a.view() {
                    ValueView::Pair(k, v) if k.as_str() == key => Some(v.clone()),
                    _ => None,
                }
            })
        });
        let bound = bound.map(|value| {
            if crate::value::attr_twigil_base(&pd.name).is_some() {
                value.deref_container()
            } else {
                value
            }
        });
        let val = match bound {
            // A `:$x is raw` the gate admitted (`method_container_args_light`)
            // and the promotion rewrote: a shared cell or a Proxy aliases the
            // caller's container, anything else is a read-only term.
            Some(v) if pd.binds_caller_container() => {
                let v = crate::runtime::types::unwrap_varref_value(v);
                if !(v.is_container_ref() || v.is_proxy_value()) {
                    outcome = NamedParamBind::RawReadonly;
                }
                v
            }
            // `@`-sigiled (attributive) named param: re-home a List into an
            // Array (mirrors `bind_param_value`); scalar named params are item
            // bindings (mirrors the slow binder's named arm).
            Some(v) if pd.name.starts_with("@!") || pd.name.starts_with("@.") => {
                Self::attributive_array_param_value(v)
            }
            Some(v) if pd.name.starts_with('@') => Self::normalize_positional_param_value(v),
            Some(v) if pd.name.starts_with('%') => v,
            Some(v) => Self::itemize_plain_scalar_param(pd, v),
            // An unsupplied optional named param binds its type-object
            // default. For an attributive param this overwrites the attribute
            // -- the slow path and rakudo both do (pinned by
            // t/tweak-named-fast-path.t).
            None => match method_def
                .named_fast(idx)
                .and_then(|plan| plan.fill.clone())
            {
                Some(default) => default,
                None if pd.default.is_some() && method_def.default_prologue(idx) => {
                    outcome = NamedParamBind::DeferredDefault;
                    Self::missing_optional_param_value(pd)
                }
                None => Self::missing_optional_param_value(pd),
            },
        };
        // `:$!x` binds by assignment: an explicit Nil stores the attribute's
        // container default (`BUILD(:$!x)` with `.new(:x(Nil))` leaves `$!x`
        // as `Any`).
        let val = if val.is_nil() && pd.name.starts_with('!') {
            self.attr_store_nil_default(owner_sym.as_str(), &pd.name[1..], '$', val)
        } else {
            val
        };
        // Attributive param: write the one attribute through `self`'s shared
        // cell (the single-key cell write `bind_param_value` performs) BEFORE
        // the frame's locals are read off the cell.
        if let Some((attr_name, is_private)) = crate::value::attr_twigil_base(&pd.name)
            && let Some(cell) = attrs_cell
        {
            // Inside a BUILD phase this bind counts as "BUILD set it", even
            // unpassed (the type-object bind must suppress the `has $.a = 5`
            // initializer; the seed comparison alone cannot tell -- an untyped
            // attribute's seed IS the same type object). The slow path records
            // this via its exit attr-local sync through
            // `write_attr_cell_by_key`; the fast binders record it here. No-op
            // outside BUILD.
            let bare_sym = Symbol::intern(attr_name);
            // A scalar and a container attribute may share a bare name
            // (`has %!c; has $!c`); resolve the storage key by sigil so `:$!c`
            // does not overwrite the `%!c` slot.
            let sigil = crate::value::attr_twigil_sigil(&pd.name).unwrap_or('$');
            let attr_sym =
                Self::attr_key_in_map(Some(owner_sym), bare_sym, is_private, sigil, &cell.as_map())
                    .unwrap_or(bare_sym);
            self.record_build_attr_write(cell, attr_sym);
            cell.insert(attr_sym, val.clone());
        }
        (val, outcome)
    }
}
