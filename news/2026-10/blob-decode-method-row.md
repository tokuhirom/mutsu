# Blob.decode is a method-table row

`Blob.decode` and `Buf.decode` are now interpreter rows of the one method table (ADR-11276 §9.33). The row and the cascade for receivers without a shape end in one decoder, `Interpreter::decode_buf`; the pure copies in the 0- and 1-argument arms and the callers' separate newline translation are gone.
