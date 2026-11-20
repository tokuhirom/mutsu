//! An explicit named slurpy (`*%h`) on the method fast binder (#18267, O2).
//!
//! A method that declares its own `*%h` gets no implicit `*%_`, and until
//! now its every call went through the general binder: a constructor
//! `method new(:$a, *%rest)` paid about 20k instructions more per call than
//! the same method without the slurpy. AttrX::Mooish's `AttrProxy.new(:$FETCH,
//! :$STORE, *%attrinit)` is the hot case.
//!
//! The general binder collects into `%h` every named argument no explicit
//! named parameter consumes. A leftover argument the call site wrote as a
//! variable (`:$instance`, `:key($v)`) keeps that variable's container
//! (`capture_named_container`): in raku `%h<key> = 7` assigns the caller's
//! `$v`, and `$!x := %h<key>` binds it. The fast path classifies each leftover
//! argument the way a named `is raw` parameter's argument is classified
//! (`vm_method_container_params`), and admits the call only when every
//! leftover has a bind it reproduces:
//!
//! * a value, or a frame fixture such as `self`, is stored as it is;
//! * a shared cell (the argument itself, or the one the caller's slot or a
//!   captured variable holds) is stored as that cell;
//! * a caller lexical still held as a plain value has its slot promoted to a
//!   shared cell, which is stored. The general binder installs a fresh cell
//!   and writes it back at exit; both leave the caller's variable aliased to
//!   the hash element.
//!
//! A `Proxy` argument, and every source the classification cannot settle,
//! keeps the general binder. The promotion runs while the caller's frame is
//! current, and after the call's deferral frame has recorded the arguments
//! as they arrived, so `callsame` forwards the caller's values.

use super::*;
use crate::vm::vm_method_container_params::MethodContainerBind;

impl Interpreter {
    /// Whether `pd` is a plain explicit named slurpy the fast binder fills:
    /// `*%h` with no type, `where`, sub-signature or trait, and not the
    /// implicit `*%_`.
    // Cost: O(1).
    pub(crate) fn method_named_slurpy_light(pd: &crate::ast::ParamDef) -> bool {
        pd.slurpy
            && !pd.double_slurpy
            && !pd.sigilless
            && !pd.is_invocant
            && pd.name.len() > 1
            && pd.name.starts_with('%')
            && pd.name != "%_"
            && pd
                .name
                .as_bytes()
                .get(1)
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
            && pd.type_constraint.is_none()
            && pd.type_capture.is_none()
            && pd.where_constraint.is_none()
            && pd.sub_signature.is_none()
            && pd.outer_sub_signature.is_none()
            && pd.code_signature.is_none()
            && pd.literal_value.is_none()
            && pd.traits.is_empty()
            && pd.trait_args.is_empty()
    }

    /// Whether `method_def` declares a light explicit named slurpy.
    // Cost: O(p), p = parameters.
    pub(crate) fn method_has_light_named_slurpy(method_def: &crate::runtime::MethodDef) -> bool {
        method_def
            .param_defs
            .iter()
            .any(Self::method_named_slurpy_light)
    }

    /// The leftover named arguments of a call: `(index, key, value)` of every
    /// string-keyed Pair no explicit named parameter consumes.
    // Cost: O(a * p), a = arguments, p = parameters.
    fn named_slurpy_leftovers(
        method_def: &crate::runtime::MethodDef,
        args: &[Value],
    ) -> Vec<(usize, String, Value)> {
        let mut out = Vec::new();
        for (idx, arg) in args.iter().enumerate() {
            let unwrapped = arg.unwrap_varref();
            if !unwrapped.is_string_pair_value() {
                continue;
            }
            if let ValueView::Pair(key, val) = unwrapped.view()
                && !key.is_empty()
                && !Self::named_arg_consumed(&method_def.param_defs, key)
            {
                out.push((idx, key.to_string(), val.clone()));
            }
        }
        out
    }

    /// Whether every leftover named argument of this call binds into the
    /// method's explicit `*%h` on the fast path (see the module docs). Reads
    /// the call-site sources and the caller's slots; changes nothing.
    // Cost: O(a * p), a = arguments, p = parameters, plus O(1) env probes
    // per leftover argument.
    pub(crate) fn method_named_slurpy_args_light(
        &self,
        method_def: &crate::runtime::MethodDef,
        args: &[Value],
    ) -> bool {
        if !Self::method_has_light_named_slurpy(method_def) {
            return true;
        }
        Self::named_slurpy_leftovers(method_def, args)
            .iter()
            .all(|(idx, _, value)| {
                matches!(
                    self.method_container_arg_classify(true, *idx, value, args.len()),
                    Some(
                        MethodContainerBind::Readonly
                            | MethodContainerBind::Relay
                            | MethodContainerBind::RelayHeld(_)
                            | MethodContainerBind::RelayCaptured
                            | MethodContainerBind::Promote(_)
                    )
                )
            })
    }

    /// Rewrite the leftover named arguments of an admitted call so each Pair
    /// carries what `*%h` stores: the caller variable's shared cell (its slot
    /// promoted when it held a plain value), or the value. Runs while the
    /// caller's frame is current.
    // Cost: O(a * p), plus one cell promotion per promoted argument.
    pub(crate) fn promote_method_named_slurpy_args(
        &mut self,
        method_def: &crate::runtime::MethodDef,
        args: &mut [Value],
    ) {
        if !Self::method_has_light_named_slurpy(method_def) {
            return;
        }
        let arg_count = args.len();
        for (idx, key, value) in Self::named_slurpy_leftovers(method_def, args) {
            let bind = self.method_container_arg_classify(true, idx, &value, arg_count);
            // A value or a cell is already what the hash stores.
            if matches!(
                bind,
                Some(MethodContainerBind::Readonly | MethodContainerBind::Relay)
            ) {
                continue;
            }
            let promoted = self.promote_classified_container_arg(bind, true, idx, value);
            args[idx] = Value::pair(key, promoted);
        }
    }
}
