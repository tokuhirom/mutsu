unit module UseInBlockIncrementLexical;

# User candidates of the built-in increment and decrement operators, for
# t/modules/import-export/use-in-block-binds-increments-lexically.t
class Ctr is export {
    has $.n;
    method succ { Ctr.new(n => $!n + 1) }
}
multi sub postfix:<++>(Ctr $a is rw) is export { 'custom-postinc' }
multi sub prefix:<++>(Ctr $a is rw) is export { 'custom-preinc' }
multi sub postfix:<-->(Ctr $a is rw) is export { 'custom-postdec' }
multi sub prefix:<-->(Ctr $a is rw) is export { 'custom-predec' }
