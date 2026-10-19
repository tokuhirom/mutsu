my package MyPackageLexicalState { my Str $tz; }

sub EXPORT {
    sub get-tz { $MyPackageLexicalState::tz }
    sub put-tz($v) { $MyPackageLexicalState::tz = $v }
    Map.new: '&get-tz' => &get-tz, '&put-tz' => &put-tz;
}
