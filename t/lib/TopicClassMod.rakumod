module TopicClassMod {
    # The class makes the module body leave extra env keys; the package-block
    # exit must not record per-frame specials (the topic `$_`) into
    # `package_lexicals`, or every later call of these subs reads a stale topic.
    class Helper {
        has $.x;
    }

    our sub map-topic() {
        <a b>.map({ "m:" ~ $_ }).list
    }

    our sub for-topic() {
        my @r;
        for <a b> { @r.push("f:" ~ $_) }
        @r.List
    }

    our sub grep-topic() {
        <a b a>.grep({ $_ eq "a" }).elems
    }
}
