module HookTopicSub {
    our sub tj(\obj) {
        my @out;
        sub jsonify(\o --> Nil) {
            with o { @out.push: "<{$_}>" }
        }
        jsonify(obj);
        @out.join
    }
}
sub EXPORT() {
    my sub unused() { 1 }
    Map.new(('&tj' => &HookTopicSub::tj,))
}
