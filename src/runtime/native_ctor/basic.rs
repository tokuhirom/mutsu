//! Native constructors: basic value types (`Int`, `Str`, allomorphs, ...).

use super::CtorCall;
use crate::runtime::*;

impl Interpreter {
    /// `Int`.
    pub(super) fn ctor_int(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Self::build_native_int_value(&args)
    }

    /// `Num`.
    pub(super) fn ctor_num(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Self::build_native_num_value(&args)
    }

    /// `Str`: the default constructor ignores positional args and yields the
    /// empty string (mutsu is lenient where raku rejects a positional).
    /// `Bool` is intentionally not here: it is an enum, so `Bool.new` errors in
    /// `dispatch_new` before the basic-type arm.
    pub(super) fn ctor_str(
        &mut self,
        _c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::str(String::new()))
    }

    /// `IntStr`, `NumStr`, `RatStr`, `ComplexStr`: `.new(numeric, string)` is
    /// pure data assembly (a numeric value mixed with a `Str` override).
    pub(super) fn ctor_allomorph(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Self::build_native_allomorph_value(&c.class_name.resolve(), &args)
    }

    /// `ObjAt`, `ValueObjAt`: stores the stringified first positional as `WHICH`.
    pub(super) fn ctor_objat(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Self::build_native_objat_value(c.class_name, &args)
    }

    /// `Failure` (reads `$!` / the MRO).
    pub(super) fn ctor_failure(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Ok(self.build_native_failure_value(&args))
    }

    /// `Capture.new` is named-only (`Mu.new`): a positional arg dies. Its build
    /// signature is `:@list, :%hash`, so a `list`/`hash` named arg populates the
    /// Capture's positional/named parts; every other named arg is dropped (bless
    /// ignores unknown attributes), yielding an empty `\()`.
    pub(super) fn ctor_capture(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.iter().any(|a| !a.is_string_pair_value()) {
            return Err(RuntimeError::new(
                "Default constructor for 'Capture' only takes named arguments",
            ));
        }
        let mut positional = Vec::new();
        let mut named = ValueMap::default();
        for a in &args {
            if let ValueView::Pair(k, v) = a.view() {
                match k.as_str() {
                    "list" => positional = Self::value_to_list(v),
                    "hash" => {
                        if let ValueView::Hash(h) = v.view() {
                            for (hk, hv) in h.iter() {
                                named.insert(hk.clone(), hv.clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(Value::capture(positional, named))
    }
}
