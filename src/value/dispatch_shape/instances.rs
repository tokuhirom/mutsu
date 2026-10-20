//! The built-in classes whose instances have a dispatch shape, by interned
//! class name (`DispatchShape::from_instance_class`).

use super::DispatchShape;
use crate::symbol::Symbol;
use std::sync::OnceLock;

/// The shapes whose values are `Instance`s of a built-in class, by interned
/// class name: a compare of two ids, not of two strings.
pub(super) fn instance_shapes() -> &'static [(Symbol, DispatchShape); 38] {
    static SHAPES: OnceLock<[(Symbol, DispatchShape); 38]> = OnceLock::new();
    SHAPES.get_or_init(|| {
        [
            (Symbol::intern("Date"), DispatchShape::Date),
            (Symbol::intern("Label"), DispatchShape::Label),
            (Symbol::intern("Format"), DispatchShape::Format),
            (
                Symbol::intern("CompUnit::DependencySpecification"),
                DispatchShape::CompUnitDepSpec,
            ),
            (
                Symbol::intern("IO::Path::Parts"),
                DispatchShape::IoPathParts,
            ),
            (Symbol::intern("Backtrace"), DispatchShape::Backtrace),
            (
                Symbol::intern("Backtrace::Frame"),
                DispatchShape::BacktraceFrame,
            ),
            (Symbol::intern("DateTime"), DispatchShape::DateTime),
            (Symbol::intern("Instant"), DispatchShape::Instant),
            (Symbol::intern("Duration"), DispatchShape::Duration),
            (Symbol::intern("Match"), DispatchShape::Match),
            (Symbol::intern("IO::Path"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Unix"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Win32"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::Cygwin"), DispatchShape::IoPath),
            (Symbol::intern("IO::Path::QNX"), DispatchShape::IoPath),
            (Symbol::intern("IO::Handle"), DispatchShape::IoHandle),
            (Symbol::intern("Blob"), DispatchShape::Blob),
            (Symbol::intern("Buf"), DispatchShape::Buf),
            (Symbol::intern("utf8"), DispatchShape::Blob),
            (Symbol::intern("utf16"), DispatchShape::Blob),
            (Symbol::intern("utf32"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint8]"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint16]"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint32]"), DispatchShape::Blob),
            (Symbol::intern("Blob[uint64]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int8]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int16]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int32]"), DispatchShape::Blob),
            (Symbol::intern("Blob[int64]"), DispatchShape::Blob),
            (Symbol::intern("Buf[uint8]"), DispatchShape::Buf),
            (Symbol::intern("Buf[uint16]"), DispatchShape::Buf),
            (Symbol::intern("Buf[uint32]"), DispatchShape::Buf),
            (Symbol::intern("Buf[uint64]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int8]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int16]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int32]"), DispatchShape::Buf),
            (Symbol::intern("Buf[int64]"), DispatchShape::Buf),
        ]
    })
}
