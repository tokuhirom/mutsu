unit module UseInBlockBuiltinOpLexical;

# User candidates of built-in operators, for
# t/modules/import-export/use-in-block-binds-builtin-operator-candidates-lexically.t
class Pt is export {
    has $.x;
    method gist { "Pt($!x)" }
}
multi sub infix:<+>(Pt $a, Pt $b) is export { Pt.new(x => $a.x + $b.x) }
multi sub infix:<*>(Pt $a, Int $n) is export { Pt.new(x => $a.x * $n) }
multi sub infix:<==>(Pt $a, Pt $b) is export { $a.x == $b.x }
multi sub infix:<cmp>(Pt $a, Pt $b) is export { $b.x cmp $a.x }
multi sub infix:<max>(Pt $a, Pt $b) is export { 'custom-max' }
multi sub prefix:<->(Pt $a) is export { Pt.new(x => -$a.x) }
multi sub prefix:<~>(Pt $a) is export { 'str-pt' }
multi sub infix:<->(UInt $a, UInt $b) is export { callsame() + 100 }
multi sub infix:<%>(Int $a, Int $b) is export { 'ambiguous with the core' }
