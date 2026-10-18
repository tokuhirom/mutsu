unit module UseInBlockRepeatPostfixLexical;

# User candidates of the built-in repetition operators, a built-in postfix
# and the built-in postcircumfixes, for
# t/modules/import-export/use-in-block-binds-repeat-postfix-lexically.t
class Rx is export {
    has $.n;
    method Int { 2 }
    method Str { 'r' }
    method AT-KEY($k) { "built-in-at-key-$k" }
}
multi sub infix:<x>(Rx $a, Rx $b) is export { 'custom-x' }
multi sub infix:<xx>(Rx $a, Rx $b) is export { 'custom-xx' }
multi sub postfix:<i>(Rx $a) is export { 'custom-i' }
multi sub postcircumfix:<[ ]>(Rx $a, $i) is export { "custom-at-pos-$i" }
multi sub postcircumfix:<{ }>(Rx $a, $k) is export { "custom-at-key-$k" }
