my class PcRoundedLib::Rounded is Array { }
my constant Rounded is export = PcRoundedLib::Rounded;

my constant &old-same = &postcircumfix:<[ ]>;
proto sub postcircumfix:<[ ]>($, |) is export {*}

multi sub postcircumfix:<[ ]>(PcRoundedLib::Rounded:D \SELF, Any:D \index) {
    old-same SELF, index.round
}
multi sub postcircumfix:<[ ]>(PcRoundedLib::Rounded:D \SELF, Any:D \index, |c) {
    old-same SELF, index.round, |c
}
multi sub postcircumfix:<[ ]>(Array:D \SELF, Any:D \index) {
    old-same SELF, index
}
multi sub postcircumfix:<[ ]>(Array:D \SELF, Any:D \index, |c) {
    old-same SELF, index, |c
}
