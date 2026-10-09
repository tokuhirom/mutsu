//! Native constructors: io temporal family (lifted from `dispatch_new_unallocated`).

use super::CtorCall;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `IO::CatHandle`.
    pub(super) fn ctor_io_cathandle(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            Ok(self.build_io_cathandle(*class_name, &args))
    }

    /// `Date`.
    pub(super) fn ctor_date(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_key = c.class_key;
            // An augmented `multi method new` (MONKEY-TYPING) wins when
            // a candidate matches; a no-match falls back to the native
            // ctor (mirrors the user-new dispatch further below).
            if let Some(result) = self.try_augmented_builtin_new(class_key, &args)? {
                return Ok(result);
            }
            // Shared with the VM's native fast path.
            let args = self.fetch_proxy_ctor_args(&args)?;
            Self::build_native_date(&args)
    }

    /// `DateTime`.
    pub(super) fn ctor_datetime(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_key = c.class_key;
            if let Some(result) = self.try_augmented_builtin_new(class_key, &args)? {
                return Ok(result);
            }
            // Shared with the VM's native fast path.
            let args = self.fetch_proxy_ctor_args(&args)?;
            Self::build_native_datetime(&args)
    }

    /// `IO::Socket::INET`.
    pub(super) fn ctor_io_socket_inet(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared single implementation with the VM's native fast
            // path. The real bind/connect writes only VM-owned
            // `io_handles` state (same shape as the native `IO::Path.open`).
            self.dispatch_socket_inet_new(&args)
    }

    /// `Proc::Async`.
    pub(super) fn ctor_proc_async(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            // Shared single implementation with the VM's native fast
            // path (`try_native_builtin_construct`). Pure data assembly:
            // arg parsing + process-global supply ids + empty Supply
            // attributes. The process is only spawned later by `.start`.
            Self::build_native_proc_async_value(*class_name, &args)
    }
}
