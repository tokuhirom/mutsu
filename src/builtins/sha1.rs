//! SHA-1 (FIPS 180-4).
//!
//! Kept in-tree rather than pulled in as a crate: the only consumer is the
//! `nqp::sha1` op, the algorithm is fixed and tiny, and the wasm build has no
//! room for an optional-dependency matrix over one hash function.

/// The 20-byte SHA-1 digest of `data`.
pub(crate) fn sha1_digest(data: &[u8]) -> [u8; 20] {
    let mut h: [u32; 5] = [
        0x6745_2301,
        0xEFCD_AB89,
        0x98BA_DCFE,
        0x1032_5476,
        0xC3D2_E1F0,
    ];
    let bit_len = (data.len() as u64).wrapping_mul(8);

    let mut chunks = data.chunks_exact(64);
    for block in &mut chunks {
        compress(&mut h, block);
    }

    // Padding: 0x80, then zeros, then the 64-bit big-endian bit length. It
    // spills into a second block when the remainder leaves no room for both.
    let rest = chunks.remainder();
    let mut tail = [0u8; 128];
    tail[..rest.len()].copy_from_slice(rest);
    tail[rest.len()] = 0x80;
    let tail_len = if rest.len() + 1 + 8 <= 64 { 64 } else { 128 };
    tail[tail_len - 8..tail_len].copy_from_slice(&bit_len.to_be_bytes());
    for block in tail[..tail_len].chunks_exact(64) {
        compress(&mut h, block);
    }

    let mut out = [0u8; 20];
    for (slot, word) in out.chunks_exact_mut(4).zip(h.iter()) {
        slot.copy_from_slice(&word.to_be_bytes());
    }
    out
}

/// The SHA-1 digest of `data` as 40 uppercase hex digits — the shape nqp's
/// `sha1` op returns.
pub(crate) fn sha1_hex_uppercase(data: &[u8]) -> String {
    let mut out = String::with_capacity(40);
    for byte in sha1_digest(data) {
        out.push(hex_upper(byte >> 4));
        out.push(hex_upper(byte & 0x0F));
    }
    out
}

fn hex_upper(nibble: u8) -> char {
    match nibble {
        0..=9 => (b'0' + nibble) as char,
        _ => (b'A' + nibble - 10) as char,
    }
}

/// One 64-byte block through the compression function. `block` is always
/// exactly 64 bytes (both call sites feed it from `chunks_exact(64)`).
fn compress(h: &mut [u32; 5], block: &[u8]) {
    let mut w = [0u32; 80];
    for (word, chunk) in w.iter_mut().zip(block.chunks_exact(4)) {
        *word = u32::from_be_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
    }
    for i in 16..80 {
        w[i] = (w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16]).rotate_left(1);
    }

    let (mut a, mut b, mut c, mut d, mut e) = (h[0], h[1], h[2], h[3], h[4]);
    for (i, wi) in w.iter().enumerate() {
        let (f, k) = match i {
            0..=19 => ((b & c) | (!b & d), 0x5A82_7999u32),
            20..=39 => (b ^ c ^ d, 0x6ED9_EBA1),
            40..=59 => ((b & c) | (b & d) | (c & d), 0x8F1B_BCDC),
            _ => (b ^ c ^ d, 0xCA62_C1D6),
        };
        let temp = a
            .rotate_left(5)
            .wrapping_add(f)
            .wrapping_add(e)
            .wrapping_add(k)
            .wrapping_add(*wi);
        e = d;
        d = c;
        c = b.rotate_left(30);
        b = a;
        a = temp;
    }

    h[0] = h[0].wrapping_add(a);
    h[1] = h[1].wrapping_add(b);
    h[2] = h[2].wrapping_add(c);
    h[3] = h[3].wrapping_add(d);
    h[4] = h[4].wrapping_add(e);
}

#[cfg(test)]
mod tests {
    use super::sha1_hex_uppercase;

    fn hex(s: &str) -> String {
        sha1_hex_uppercase(s.as_bytes())
    }

    #[test]
    fn nist_vectors() {
        assert_eq!(hex(""), "DA39A3EE5E6B4B0D3255BFEF95601890AFD80709");
        assert_eq!(hex("abc"), "A9993E364706816ABA3E25717850C26C9CD0D89D");
        assert_eq!(
            hex("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
            "84983E441C3BD26EBAAE4AA1F95129E5E54670F1"
        );
    }

    #[test]
    fn padding_block_boundaries() {
        // 55 bytes: length still fits in the first block. 56 and 64: it does
        // not, so padding spills into a second block.
        assert_eq!(
            hex(&"a".repeat(55)),
            "C1C8BBDC22796E28C0E15163D20899B65621D65A"
        );
        assert_eq!(
            hex(&"a".repeat(56)),
            "C2DB330F6083854C99D4B5BFB6E8F29F201BE699"
        );
        assert_eq!(
            hex(&"a".repeat(64)),
            "0098BA824B5C16427BD7A1122A5A442A25EC644D"
        );
    }

    #[test]
    fn million_a_vector() {
        assert_eq!(
            hex(&"a".repeat(1_000_000)),
            "34AA973CD4C4DAA4F61EEB2BDBAD27316534016F"
        );
    }

    #[test]
    fn hashes_utf8_bytes_not_codepoints() {
        // nqp::sha1 digests the string's UTF-8 encoding.
        assert_eq!(hex("日本語"), "C12140A0FFB4E56481B4FE0A7A25040C2EAFA9CA");
        assert_eq!(hex("𝄞x"), "116693A63839D657C9461C06193DA339DFFF4EF3");
    }
}
