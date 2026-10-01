# A `sub EXPORT` module whose hook body ENDS in a bare block: the block's value
# is the hook's return value, so the `my \oui` and `sub infix:<puis>` declared
# in it are exported through the `Map` it builds.
use v6.d;

sub EXPORT(|) {
    {
        my \oui = True;
        sub infix:<puis>($a, $b) { "$a,$b" }
        Map.new('oui' => oui, '&infix:<puis>' => &infix:<puis>);
    }
}
