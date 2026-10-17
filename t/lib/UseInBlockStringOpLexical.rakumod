unit module UseInBlockStringOpLexical;

# User candidates of the built-in string operators, for
# t/modules/import-export/use-in-block-binds-string-operators-lexically.t
class Tx is export {
    has $.s;
    method Str { "Tx($!s)" }
}
multi sub infix:<~>(Tx $a, Tx $b) is export { Tx.new(s => $a.s ~ '+' ~ $b.s) }
multi sub infix:<eq>(Tx $a, Tx $b) is export { 'custom-eq' }
multi sub infix:<lt>(Tx $a, Tx $b) is export { $a.s gt $b.s }
multi sub infix:<leg>(Tx $a, Tx $b) is export { $b.s leg $a.s }
