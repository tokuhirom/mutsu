# RakuAST: attribute `will` traits

`has $.v will lazy { ... }` now renders as `RakuAST::Trait::Will` (phase and
block) in the attribute's traits, and EVAL of such a tree lowers it back so the
user's `trait_mod:<will>` still receives the block. Two more `t/` files pass
under `MUTSU_RAKUAST=1` (ADR-10723 section 6.55).
