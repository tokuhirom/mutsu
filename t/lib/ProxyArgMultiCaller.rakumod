unit module ProxyArgMultiCaller;

our sub call-op(Mu $got is raw, $op, Mu $expected is raw) is export {
    $op($got, $expected)
}
