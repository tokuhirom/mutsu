# The EVAL($ast) ratchet pins 564 more files

After the EVAL fixes of #15873, #15885 and #15886, `scripts/rakuast-frontend.sh eval-update` found 564 more `t/` files that pass
when run as `EVAL(slurp($file).AST)`, measured with a release build and a cold precompile cache (as CI runs it). The pinned list
`ci/rakuast-ast-eval-passing.txt` grows from 8342 to 8906 files, so CI now fails if any of them stops passing through the write
direction of the RakuAST layer.
