package ExportOverBase {
    our sub shout(\obj, :$k) { "base({obj})" }
}
sub EXPORT(*@_) {
    Map.new: '&shout' => &ExportOverBase::shout;
}
