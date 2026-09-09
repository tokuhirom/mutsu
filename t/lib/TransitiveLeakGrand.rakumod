use TransitiveLeakOuter;
unit module TransitiveLeakGrand;

# This module `use`s TransitiveLeakOuter, which itself `use`s
# TransitiveLeakInner. Inner's classes are two hops away and must not be
# resolvable bare from here -- not even from a method body, which resolves
# bare names through its own class's package chain rather than through `env`.
class GrandHolder is export {
    method peek-inner() {
        my $c = ::('InnerClass');
        $c ~~ Failure ?? 'MISSING' !! $c.^name;
    }
    method peek-outer() { OuterClass.^name }
}
sub grand-probe() is export { GrandHolder.new.peek-inner }
sub grand-outer-probe() is export { GrandHolder.new.peek-outer }
