//! Object-lifetime pinning for NativeCall `Blob`/`Buf` arguments.
//!
//! A `Blob` argument is passed to C as a `void*` to its bytes. mutsu stores a
//! `Buf`'s bytes as an `Array` of boxed `Int` values, not as contiguous C
//! memory, so the address handed to C has to come from a separate byte buffer.
//! Copying into a buffer that only lives for the duration of the call is wrong
//! for any C function that *retains* the pointer — OpenSSL's
//! `BIO_new_mem_buf(buf, len)` is exactly that: it builds a read-only memory
//! BIO **over the caller's memory** and reads it later, so a per-call temporary
//! left the BIO pointing at freed memory (the PEM parse then returned NULL and
//! `RSA_size(NULL)` segfaulted). The library that does this documents the
//! requirement — OpenSSL.rakumod comments that "`$bio-buf` contains a C pointer
//! to `$private-pem-buf`, so `$private-pem-buf` needs to stay alive as long as
//! `$bio-buf` does" — and in Rakudo it is satisfied for free, because a Blob's
//! storage *is* the C buffer.
//!
//! So mutsu pins the buffer to the Raku object instead: the first native call
//! that passes a given `Buf` allocates its C-side bytes here, keyed by the
//! object's attribute-cell address, and every later call reuses (and refreshes)
//! that same allocation. The entry is released from `Drop for InstanceAttrs`,
//! so the C buffer lives exactly as long as the Raku `Buf` that owns it — no
//! leak, and no dangling pointer while the object is still reachable.
//!
//! TODO: the real fix is to give `Buf`/`Blob` a native contiguous byte
//! representation, so its storage can be handed to C directly the way Rakudo
//! does, and this mirror can go away.

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Live pin count. `release` is called from every `InstanceAttrs` drop — a very
/// hot path — so it checks this first and skips the lock entirely for the
/// overwhelmingly common case of a program that pins nothing.
static PINNED_COUNT: AtomicUsize = AtomicUsize::new(0);

fn registry() -> &'static Mutex<HashMap<usize, Box<[u8]>>> {
    static REGISTRY: std::sync::OnceLock<Mutex<HashMap<usize, Box<[u8]>>>> =
        std::sync::OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Pin `bytes` for the object identified by `key` and return the stable address
/// of the pinned copy. Reuses the existing allocation when the length is
/// unchanged (the usual case: the same `Buf` handed to C again), so a pointer C
/// already retained keeps pointing at the live bytes.
///
/// Returns `None` only if the registry mutex is poisoned, in which case the
/// caller falls back to a per-call temporary.
pub(crate) fn pin(key: usize, bytes: &[u8]) -> Option<*mut u8> {
    let mut map = registry().lock().ok()?;
    let slot = match map.get_mut(&key) {
        Some(existing) if existing.len() == bytes.len() => existing,
        _ => {
            if map.insert(key, bytes.to_vec().into_boxed_slice()).is_none() {
                PINNED_COUNT.fetch_add(1, Ordering::Relaxed);
            }
            map.get_mut(&key)?
        }
    };
    slot.copy_from_slice(bytes);
    Some(slot.as_mut_ptr())
}

/// Read back the pinned bytes for `key` — the C side may have written into
/// them (an out-buffer such as `SSL_read` / `BIO_read`).
pub(crate) fn read(key: usize) -> Option<Vec<u8>> {
    registry().lock().ok()?.get(&key).map(|b| b.to_vec())
}

/// Free the pinned buffer owned by `key`. Called from `Drop for InstanceAttrs`
/// while the attribute cell is still alive, so the key cannot yet have been
/// recycled by a later allocation.
pub(crate) fn release(key: usize) {
    if PINNED_COUNT.load(Ordering::Relaxed) == 0 {
        return;
    }
    if let Ok(mut map) = registry().lock()
        && map.remove(&key).is_some()
    {
        PINNED_COUNT.fetch_sub(1, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pinned_address_is_stable_across_repins_of_the_same_length() {
        let key = 0x1000;
        let first = pin(key, b"hello").unwrap();
        let second = pin(key, b"world").unwrap();
        assert_eq!(
            first, second,
            "same-length re-pin must reuse the allocation"
        );
        assert_eq!(read(key).unwrap(), b"world".to_vec());
        release(key);
        assert!(read(key).is_none());
    }

    #[test]
    fn release_is_cheap_and_idempotent_for_unpinned_keys() {
        release(0x2000);
        release(0x2000);
        assert!(read(0x2000).is_none());
    }
}
