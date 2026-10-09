//! Native constructors: numeric family (lifted from `dispatch_new_unallocated`).

use super::CtorCall;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `Version`.
    pub(super) fn ctor_version(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            let arg = args.first().cloned().unwrap_or(Value::NIL);
            Ok(Self::version_from_value(arg))
    }

    /// `Duration`.
    pub(super) fn ctor_duration(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path (pure Rational build;
            // a bad string arg is the one fallible built-in builder).
            Self::build_native_duration_value(&args)
    }

    /// `StrDistance`.
    pub(super) fn ctor_strdistance(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path.
            Ok(Self::build_native_strdistance_value(&args))
    }

    /// `utf8`, `utf16`.
    pub(super) fn ctor_utf8(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            // Shared with the VM's native fast path (pure code-unit build).
            Ok(Self::build_native_utf_value(*class_name, &args))
    }

    /// `Buf`, `buf8`, `Buf[uint8]`, `Blob`, `blob8`, `Blob[uint8]`, `buf16`, `buf32`, `buf64`, `blob16`, `blob32`, `blob64`.
    pub(super) fn ctor_buf(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            // Shared with the VM's native fast path (the byte-overlay
            // build is pure data; see `build_native_buf_value`).
            Ok(Self::build_native_buf_value(*class_name, &args))
    }

    /// `Rat`.
    pub(super) fn ctor_rat(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path (pure component build).
            Ok(Self::build_native_rat_value(&args))
    }

    /// `FatRat`.
    pub(super) fn ctor_fatrat(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path.
            Ok(Self::build_native_fatrat_value(&args))
    }

    /// `Complex`.
    pub(super) fn ctor_complex(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path (pure component build).
            Ok(Self::build_native_complex_value(&args))
    }
}
