The RakuAST regex boundary now preserves simple bare subrules (`<name>`) and
dot-suppressed subrules (`<.name>`) as structured named assertions. Their
execution continues through the existing context-sensitive matcher so builtin
assertions and grammar-local subrule overrides retain their behavior.
