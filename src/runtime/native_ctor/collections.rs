//! Native constructors: collections family (lifted from `dispatch_new_unallocated`).

use super::CtorCall;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `IterationBuffer`.
    pub(super) fn ctor_iterationbuffer(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            // Shared with the VM's native fast path.
            Ok(Self::build_native_iterationbuffer_value(*class_name, &args))
    }

    /// `Array`, `List`, `Positional`, `array`, `CArray`.
    pub(super) fn ctor_array(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
        let base_class_name = c.base_class_name;
        let type_args = c.type_args.clone();
            // Shared single implementation with the VM's native fast path.
            self.try_native_array_construct(
                *class_name,
                base_class_name,
                &type_args,
                &args,
            )
    }

    /// `Hash`, `Map`.
    pub(super) fn ctor_hash(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
        let type_args = c.type_args.clone();
            // Shared single implementation with the VM's native fast path.
            self.try_native_hash_construct(*class_name, &type_args, &args)
    }

    /// `Uni`.
    pub(super) fn ctor_uni(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path (pure codepoint build).
            Ok(Self::build_native_uni_value(&args))
    }

    /// `Seq`.
    pub(super) fn ctor_seq(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared single implementation with the VM's native fast path
            // (`try_native_seq_construct`). Reads/writes only VM-owned state
            // (the `predictive_seq_iters` carrier table + the deferred-iter
            // side table keyed off the Seq's own Arc).
            Ok(self.try_native_seq_construct(&args))
    }

    /// `Pair`.
    pub(super) fn ctor_pair(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path.
            Self::build_native_pair_value(&args)
    }

    /// `Set`, `SetHash`.
    pub(super) fn ctor_set(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
        let base_class_name = c.base_class_name;
        let type_args = c.type_args.clone();
            // Native QuantHash construction — single impl shared with the
            // VM's `.new` fast path (`try_native_quanthash_construct`).
            self.try_native_quanthash_construct(
                *class_name,
                base_class_name,
                &type_args,
                args,
            )
    }

    /// `Bag`, `BagHash`.
    pub(super) fn ctor_bag(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
        let base_class_name = c.base_class_name;
        let type_args = c.type_args.clone();
            // Native QuantHash construction — single impl shared with the
            // VM's `.new` fast path (`try_native_quanthash_construct`).
            self.try_native_quanthash_construct(
                *class_name,
                base_class_name,
                &type_args,
                args,
            )
    }

    /// `Mix`, `MixHash`.
    pub(super) fn ctor_mix(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
        let base_class_name = c.base_class_name;
        let type_args = c.type_args.clone();
            // Native QuantHash construction — single impl shared with the
            // VM's `.new` fast path (`try_native_quanthash_construct`).
            self.try_native_quanthash_construct(
                *class_name,
                base_class_name,
                &type_args,
                args,
            )
    }

    /// `Slip`.
    pub(super) fn ctor_slip(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path.
            Ok(Value::slip(args.to_vec()))
    }

    /// `Match`.
    pub(super) fn ctor_match(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared single implementation with the VM's native fast path.
            Ok(Self::build_native_match_value(&args))
    }

    /// `Junction`.
    pub(super) fn ctor_junction(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Junction.new has exactly two candidates:
            //   new(Str:D $type, \values)       — type string + one positional
            //   new(\values, Str:D :$type!)      — one positional + named :type
            // Anything else (no type, extra positionals, no args) resolves
            // to no candidate and must throw X::Multi::NoMatch rather than
            // defaulting to `any()` (roast .../multi-no-match.t).
            let mut type_str: Option<String> = None;
            let mut positional: Vec<&Value> = Vec::new();
            for arg in &args {
                if let ValueView::Pair(key, value) = arg.view() {
                    if key == "type" {
                        type_str = Some(value.to_string_value());
                    }
                } else {
                    positional.push(arg);
                }
            }
            let type_name = if let Some(t) = type_str {
                // Named :type — need exactly one positional (the values).
                if positional.len() != 1 {
                    return Err(
                        super::methods_signature_errors::make_multi_no_match_error("new"),
                    );
                }
                t
            } else if positional.len() == 2
                && matches!(positional[0].view(), ValueView::Str(_))
            {
                // Positional type string + values.
                let name = positional[0].to_string_value();
                positional.remove(0);
                name
            } else {
                return Err(super::methods_signature_errors::make_multi_no_match_error(
                    "new",
                ));
            };
            let kind = match type_name.as_str() {
                "all" => JunctionKind::All,
                "one" => JunctionKind::One,
                "none" => JunctionKind::None,
                _ => JunctionKind::Any,
            };
            let values_arg = positional.first().copied();
            // Rakudo binds `\values` and stores `values.list` as the
            // eigenstates, so EVERY iterable flattens -- a `Range`
            // (`Junction.new("one", 1..6)` is a six-eigenstate junction),
            // a `Hash`/`Set`/`Bag`/`Mix` (which list as their pairs), a
            // `Seq`, and an itemized list (`$(1,2)`) alike -- while a
            // `Str`/`Int` stays a single eigenstate. Enumerating only
            // Array/Seq/Slip wrapped a `Range` as ONE (truthy) element,
            // so `Junction.new("one", 1..6).Bool` answered True instead
            // of False. `value_to_list_for_receiver` is precisely
            // `.list` on the argument itself (it ignores the argument's
            // own itemization, which `\values` does too).
            let elems: Vec<Value> = match values_arg {
                Some(v) => crate::runtime::utils::value_to_list_for_receiver(v),
                None => vec![],
            };
            Ok(Value::junction(kind, elems))
    }
}
