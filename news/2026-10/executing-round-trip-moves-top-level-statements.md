# The executing round trip moves top-level statements instead of cloning them

The executing RakuAST round trip used to clone every carried top-level statement into its leaf and clone it out again when the statement list was lowered. A unit it owns now gives each carried statement up (`stmt_leaf_taking`), and the leaf's table slot is taken out once by the lowering. Nested leaves are still read more than once and are cloned out as before.

Frontend Ir against the frozen pre-flip baseline (ADR-15334 section 3.2): zef-cli 105.1%, s32-exceptions-misc 102.9%, s12-class-attributes 103.4%, s15-nfg-emoji-test 102.2%, grammar-cursor-attributes 109.3% (was 112.5%), so every budget case is within 110%.

The statement-list conversion moved out of `convert.rs` into `statements.rs`, which lowers the `convert-lines` ratchet from 6871 to 6790.
