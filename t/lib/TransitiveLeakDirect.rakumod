use TransitiveLeakInner;
unit module TransitiveLeakDirect;

# A sibling-package NativeCall shape: this module references a class declared
# by another module bare, from its own method body.
class Direct {
    method inner-name() { InnerClass.^name }
}
sub direct-probe() is export { Direct.new.inner-name }
