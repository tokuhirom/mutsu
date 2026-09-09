use TransitiveLeakInner;
unit module TransitiveLeakOuter;

class OuterClass is export { method who() { 'outer-class' } }

# The module's own routines must keep resolving the short names it imported,
# even though the importer of this module must not see them.
sub outer-uses-inner() is export { InnerClass.new.who ~ '/' ~ InnerConst }
class OuterHolder is export {
    method make-inner() { InnerClass.new.who }
}
