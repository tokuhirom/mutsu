unit module UseInBlockAffixOps;

sub prefix:<¢>($x) is export { "P($x)" }
sub postfix:<¥>($x) is export { "Q($x)" }
sub circumfix:<⌊ ⌋>($x) is export { "C($x)" }
