sub EXPORT(\ignored, &proto) {
    say &proto.^name;
    Map.new
}
