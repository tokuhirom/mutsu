# JSON `\u` escapes no longer panic on a non-ASCII character

The native JSON parser sliced the input string by byte offset to read the four hex digits of a
`\u` escape, so `"\u000é"` panicked on a char boundary (and aborted the whole wasm instance). It
now reads the four bytes directly and rejects anything that is not an ASCII hex digit with a normal
parse error. This also stops `\u+123` from being accepted as a number.
