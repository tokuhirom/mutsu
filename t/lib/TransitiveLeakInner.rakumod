unit module TransitiveLeakInner;

class InnerClass is export { method who() { 'inner-class' } }
role InnerRole is export { method tagged() { 'inner-role' } }
constant InnerConst is export = 'inner-const';
sub inner-sub() is export { 'inner-sub' }
