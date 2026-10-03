//! Splitting a string into lines on `\n`, `\r\n` and `\r` (`.lines`, a
//! Supply's `.lines`). Pure, so it lives in `value` (#10779); `builtins`
//! re-exports it.

/// Every line of `input`, with (`chomp`) or without its separator stripped.
///
/// Cost: O(n + k), n = bytes of `input`, k = lines.
pub(crate) fn split_lines_with_chomp(input: &str, chomp: bool) -> Vec<String> {
    let bytes = input.as_bytes();
    let mut lines = Vec::new();
    let mut start = 0usize;
    let mut i = 0usize;
    while i < bytes.len() {
        let sep_len = if bytes[i] == b'\n' {
            1
        } else if bytes[i] == b'\r' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                2
            } else {
                1
            }
        } else {
            i += 1;
            continue;
        };

        let end = if chomp { i } else { i + sep_len };
        lines.push(input[start..end].to_string());
        i += sep_len;
        start = i;
    }

    if start < input.len() {
        lines.push(input[start..].to_string());
    }

    lines
}
