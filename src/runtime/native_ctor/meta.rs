//! Native constructors: meta family (lifted from `dispatch_new_unallocated`).

use super::CtorCall;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `Stash`, `PseudoStash`.
    pub(super) fn ctor_stash(
        &mut self,
        c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let name = c.base_class_name;
            // A Stash is essentially a Hash but with type Stash; a
            // PseudoStash is its lexical-pad sibling and constructs the
            // same way.
            Ok(Value::make_instance(Symbol::intern(name), HashMap::new()))
    }

    /// `Proxy`.
    pub(super) fn ctor_proxy(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared single implementation with the VM's native fast path.
            Ok(Self::build_native_proxy_value(&args))
    }

    /// `Parameter`.
    pub(super) fn ctor_parameter(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // `Parameter`/`Signature` are ordinarily only materialized
            // by the runtime from a real declaration, but Raku exposes
            // both as constructible types so code can synthesize a
            // signature at runtime and reconstruct it back into
            // declaration syntax via `.perl`/`.raku` (the actual
            // argument binding always goes through the ordinary
            // declaration path once that syntax is `EVAL`ed — this
            // constructed value is never itself bound against, see
            // `Template::Classic`'s `template(Signature $sig, ...)`).
            let sig_param = crate::value::signature::sig_param_from_named_args(&args);
            Ok(crate::value::signature::make_parameter_value(
                sig_param,
                Some(self),
            ))
    }

    /// `Signature`.
    pub(super) fn ctor_signature(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // See "Parameter" above.
            let info = crate::value::signature::sig_info_from_new_args(&args);
            Ok(crate::value::signature::make_signature_value(
                info,
                Some(self),
            ))
    }

    /// `Backtrace`.
    pub(super) fn ctor_backtrace(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Backtrace.new captures the current call stack;
            // Backtrace.new($offset) skips the first $offset frames.
            let bt = self.build_backtrace_value();
            // Rakudo's own `Backtrace.new` frame heads the list.
            if let ValueView::Instance { attributes, .. } = bt.view() {
                let list = attributes
                    .as_map()
                    .get("frames")
                    .map(crate::runtime::utils::value_to_list);
                if let Some(list) = list {
                    let mut frames = vec![crate::vm::setting_frame(
                        "new",
                        Value::str("SETTING::src/core.c/Backtrace.rakumod".to_string()),
                        Value::int(96),
                    )];
                    frames.extend(list);
                    attributes.insert("frames".to_string(), Value::array(frames));
                }
            }
            let offset = args
                .first()
                .and_then(|a| match a.view() {
                    ValueView::Int(n) if n > 0 => Some(n as usize),
                    _ => None,
                })
                .unwrap_or(0);
            if offset > 0
                && let ValueView::Instance { attributes, .. } = bt.view()
            {
                // Read (and drop the map guard) before the insert below.
                let list = attributes
                    .as_map()
                    .get("frames")
                    .map(crate::runtime::utils::value_to_list);
                if let Some(list) = list {
                    let rest: Vec<Value> = list.into_iter().skip(offset).collect();
                    attributes.insert("frames".to_string(), Value::array(rest));
                }
            }
            Ok(bt)
    }

    /// `Whatever`: `.new` yields the singleton-like `*` value, as in Rakudo.
    pub(super) fn ctor_whatever(
        &mut self,
        _c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::WHATEVER)
    }

    // Types that cannot be instantiated with .new
    /// `HyperWhatever`, `Instant`.
    pub(super) fn ctor_hyperwhatever(
        &mut self,
        c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            Err(RuntimeError::new(format!(
                "X::Cannot::New: Cannot create new object of type {}",
                class_name
            )))
    }
}
