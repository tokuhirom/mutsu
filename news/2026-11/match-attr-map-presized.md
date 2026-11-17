# A materialized Match's attribute map is sized up front

`MatchNode::materialize_map` inserts up to fourteen keys into a fresh
attribute map. It used to start empty and rehash three times as it grew.
It now reserves the capacity once. Under callgrind, 600
`Cro::Uri.parse-ref` calls went from 1,251.6M to 1,239.5M instructions,
about 20k (~1%) per parse. This is another slice of #17385, after the
well-known match keys.
