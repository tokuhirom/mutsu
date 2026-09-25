# Fixture for t/routines/dispatch/proto-body-writeback-through-parent-tier.t:
# the P5each shape (#9336) -- an exported proto whose multi mixes a role
# declared inside its own body into the caller's array.
unit module ProtoMultiEachMixin;
proto sub each(|) is export {*}
multi sub each(@array is raw) {
    role EachArray {
        has int $.index;
        method INIT() { $!index = -1; self }
        method each() { ++$!index < self.elems ?? ($!index, self.AT-POS($!index)) !! Empty }
    }
    @array ~~ EachArray ?? @array.each !! (@array does EachArray).INIT.each
}
