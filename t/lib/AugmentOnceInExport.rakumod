# A `sub EXPORT` hook (ADR-0087) that augments an existing type as one of its
# effects -- `Logic::Ternary`'s real shape: `augment class Any { method
# Ternary(...) {...} }` sits unconditionally in its EXPORT body, run again on
# every `use Logic::Ternary <opt>;` (mutsu re-invokes `sub EXPORT` per import;
# see `apply_module_export`'s doc comment). Real Raku elaborates `augment` at
# COMPILE time of the enclosing code -- once, however many times that code is
# later invoked -- so the augmentation itself never repeats even though the
# rest of the hook's body does.
sub EXPORT(|) {
    use MONKEY-TYPING;
    augment class Any {
        method AugmentOnceGreet { "hi" }
    }
    Map.new();
}
