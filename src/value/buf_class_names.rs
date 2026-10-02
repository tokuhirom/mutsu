//! Class-name predicates for the `Buf`/`Blob` family (and the native-backed
//! `CArray`): pure functions of a type name, so they live beside the storage
//! they gate rather than in the runtime (issue #10779). `runtime::utils`
//! re-exports them.

/// Check if a class name represents a (mutable) Buf-like type (`Buf`, `Buf[uint8]`,
/// buf8, etc.). The encoding types (utf8/utf16/...) are immutable Blobs, not
/// Bufs, so they are excluded here (see `is_blob_like_class`).
pub(crate) fn is_buf_like_class(cn: &str) -> bool {
    matches!(cn, "Buf" | "buf8" | "buf16" | "buf32" | "buf64")
        || cn.starts_with("Buf[")
        || cn.starts_with("buf")
        || super::value_buf::buffer_class_type(cn).is_some_and(|t| t.starts_with("Buf"))
}

/// Check if a class name represents a Blob-like type (`Blob`, `Blob[uint8]`, `blob8`,
/// and the immutable encoding buffers utf8/utf16/utf32).
pub(crate) fn is_blob_like_class(cn: &str) -> bool {
    matches!(
        cn,
        "Blob" | "blob8" | "blob16" | "blob32" | "blob64" | "utf8" | "utf16" | "utf32"
    ) || cn.starts_with("Blob[")
        || cn.starts_with("blob")
        || super::value_buf::buffer_class_type(cn).is_some_and(|t| t.starts_with("Blob"))
}

/// Check if a class name represents any Buf or Blob type
pub(crate) fn is_buf_or_blob_class(cn: &str) -> bool {
    is_buf_like_class(cn) || is_blob_like_class(cn)
}

/// Whether instances of `cn` keep their elements in a native storage node — a
/// `Buf`/`Blob`, or a native-backed `CArray[T]` (ADR-0015 P2 and P3).
///
/// This is the predicate for the *element-storage mechanics* an accessor in
/// [`super::value_buf`] serves: indexing, assignment, `.elems`, `.list`,
/// iteration. It is deliberately **not** the predicate for anything that means
/// "is a byte string": a `CArray` is not a `Blob`, so `.decode`, `.subbuf`,
/// the `write-*` families, the hex `.gist` and the `Buf`/`Blob` type checks all
/// keep the narrower [`is_buf_or_blob_class`] gate.
pub(crate) fn is_native_elems_class(cn: &str) -> bool {
    is_buf_or_blob_class(cn) || super::value_carray::is_native_carray_class(cn)
}
