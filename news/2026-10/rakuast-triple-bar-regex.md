# RakuAST preserves triple-bar regex alternation

The regex source tree parser now treats a contiguous run of three pipes as one
sequential-alternation operator. RakuAST retains the same node shape as for
`||`, while runs longer than three remain unsupported. The existing matcher
continues to execute the original pattern text.
