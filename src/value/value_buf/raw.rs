//! One element of a C array that no Raku buffer owns: the storage behind a
//! `nativecast(CArray[T], $ptr)` view (#11209).
//!
//! A buffer node's bytes are Raku-owned and bounds-checked. A cast view's
//! bytes belong to C, and C arrays carry no length, so these two functions
//! read and write at `addr + idx * width` on the caller's word -- the trust
//! every NativeCall declaration is given, and the same undefined behaviour
//! Rakudo has for an index past the C array.

use super::{ElemKind, Value, decode_elem_bits, elem_bits};

/// Element `idx` of the C array of `width`-byte `kind` elements at `addr`.
/// `None` for a NULL array or an index whose byte offset overflows.
///
/// # Safety
///
/// `addr` must point at a live C array holding at least `idx + 1` elements of
/// this width.
// Cost: O(1).
pub(crate) unsafe fn read_raw_elem(
    addr: usize,
    idx: usize,
    width: u8,
    kind: ElemKind,
) -> Option<Value> {
    if addr == 0 {
        return None;
    }
    let w = width as usize;
    let at = addr.checked_add(idx.checked_mul(w)?)?;
    let mut raw = [0u8; 8];
    // SAFETY: the caller vouches that `w` bytes are readable at `at`; the copy
    // is byte-wise, so the element need not be aligned.
    unsafe { std::ptr::copy_nonoverlapping(at as *const u8, raw.as_mut_ptr(), w) };
    Some(decode_elem_bits(u64::from_le_bytes(raw), width, kind))
}

/// Store `v` as element `idx` of the C array at `addr`, encoded the way a
/// buffer of the same width and kind stores it. `None` for a NULL array or an
/// overflowing offset.
///
/// # Safety
///
/// As [`read_raw_elem`], and the memory must be writable.
// Cost: O(1).
pub(crate) unsafe fn write_raw_elem(
    addr: usize,
    idx: usize,
    width: u8,
    kind: ElemKind,
    v: &Value,
) -> Option<()> {
    if addr == 0 {
        return None;
    }
    let w = width as usize;
    let at = addr.checked_add(idx.checked_mul(w)?)?;
    let bytes = elem_bits(v, width, kind).to_le_bytes();
    // SAFETY: the caller vouches that `w` bytes are writable at `at`; the copy
    // is byte-wise, so the element need not be aligned.
    unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), at as *mut u8, w) };
    Some(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_and_writes_in_place() {
        let mut block: [i32; 3] = [10, -20, 30];
        let addr = block.as_mut_ptr() as usize;
        // SAFETY: `block` holds three live i32s.
        unsafe {
            assert_eq!(
                read_raw_elem(addr, 1, 4, ElemKind::Int).map(|v| v.to_string_value()),
                Some("-20".to_string())
            );
            write_raw_elem(addr, 2, 4, ElemKind::Int, &Value::int(99)).unwrap();
        }
        assert_eq!(block, [10, -20, 99]);
    }

    #[test]
    fn a_null_array_has_no_elements() {
        // SAFETY: a NULL address is refused before any access.
        assert!(unsafe { read_raw_elem(0, 0, 4, ElemKind::Int) }.is_none());
    }
}
