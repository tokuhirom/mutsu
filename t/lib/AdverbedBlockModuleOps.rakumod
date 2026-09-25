# A block-form module declaration carrying adverbs: the parser wraps it in a
# SyntheticBlock with its metadata, which the importer's export scan must
# walk into. Also exports an operator through `our &infix:<...> is export`
# (the PatternMatching alias idiom).
module AdverbedBlockModuleOps:auth<zef:mutsu>:ver<0.1> {
    sub infix:<⊕> ($a, $b) is export { $a + $b }
    sub infix:<apply-to> ($topic, &f) is export { f($topic) }
    our &infix:<┇> is export = &[apply-to];
    constant ANSWER is export = 42;
}
