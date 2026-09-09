unit module TransitiveLeakInner;

class InnerClass is export { method who() { 'inner-class' } }
role InnerRole is export { method tagged() { 'inner-role' } }
constant InnerConst is export = 'inner-const';
sub inner-sub() is export { 'inner-sub' }

# Not exported: a `unit module`'s own file-scope `constant`s and enum values are
# package symbols of this compunit in rakudo, so the importer must not see the
# bare names -- while this module's own routines and methods still must (#7787).
constant INNER-PRIVATE = 'inner-private';
enum InnerEnum <INNER-ALPHA INNER-BETA>;
our constant INNER-OUR = 'inner-our';

sub inner-reads-private() is export { INNER-PRIVATE ~ '/' ~ INNER-ALPHA.key ~ '/' ~ INNER-OUR }
class InnerReader is export {
    method peek() { INNER-PRIVATE ~ '/' ~ INNER-BETA.key }
}
