# Bind untyped block parameters as Mu

Pointy blocks and WhateverCode now accept Junction values through their untyped parameters. Their implicit nominal type is `Mu`, so the block body can perform the usual operation on the Junction. The separate plain-sub light-call gap for a `Mu` argument is tracked in #10878.
