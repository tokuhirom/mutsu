# RakuAST renders `proceed |c` / `succeed |c` as the plain call

The parser expands `proceed |c` / `succeed |c` into a guarded control transfer, which the RakuAST converter refused as "if/unless modifier with an else branch". It now recognizes that expansion and renders rakudo's `Call::Name::WithoutParentheses` over the slipped argument; lowering rebuilds the same guarded transfer through the parser's own helper. `t/control/last-next-slipped-args.t` joins the round-trip ratchet.
