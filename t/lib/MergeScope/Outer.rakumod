# A package-less module: everything below is its own GLOBAL merge (ADR-11136).
use MergeScope::Inner;
class MergeOuterCls {
    method v { 'outer' }
    method mk { MergeInnerCls.new }
}
our sub merge-outer-our() { 'outer-our' }
constant MERGE-OUTER-C = 5;
package MergeOuterPkg { our sub f() { 'pkg-f' } }
