unit class ResElemTopic;

# Mirrors the shape Cro::HTTP::Router's `sub resource` uses: subscript the
# %?RESOURCES pseudo-hash and topicalize the hit with `with ... -> $v {...}`
# so the body can call methods (`.IO.slurp`) on the resolved resource entry.
# `%?RESOURCES` is a synthesized pseudo-hash, not a real container stored in
# locals/env, so the `with`/`given` "element-source" writeback optimization
# (meant for a real lvalue like `with %h<k>`) must NOT treat this subscript
# as one — that optimization's by-name locals lookup finds nothing and binds
# the topic to Nil, hiding the real resource entry.
method greet(--> Str) {
    with %?RESOURCES<greeting.txt> -> $resource {
        return $resource.IO.slurp(:close).trim;
    }
    return 'no resource found';
}
