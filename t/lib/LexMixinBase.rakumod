role LexMixinTag { }
my class LexBase is Array {
    method ^parameterize(Mu:U \arr, Mu \of) { arr.^mixin(LexMixinTag) }
}
my constant LexRounded is export = LexBase;
multi sub kind(LexBase:D \s) is export { "LexBase" }
multi sub kind(Array:D \s) is export { "Array" }
