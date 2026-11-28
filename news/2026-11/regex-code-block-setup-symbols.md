# A regex code block's setup runs on symbols, and a parse's start-rule probe is memoized

Each time the matcher reaches a regex `{ ... }` block, it installs the
block's capture variables into the env and restores them afterwards: `$0`
under the key `0`, `$<name>` under `<name>`, plus `$/` and `$¢`. Every run
formatted each key as a `String` and interned it again. It also saved and
restored the old bindings by `String`, and snapshotted the whole env into a
SipHash map to find the names the body rebound. Now the keys are symbols
interned once per thread (`regex_code_env_keys`), installing and restoring
use the symbol entry points, and the snapshot is an Fx map sized up front.

Every `Grammar.parse` also asks `has_proto_token` whether its start rule is a
proto. That walked the current package's MRO and probed `proto_tokens` under
each ancestor's qualified `String` name, about 10k instructions for a grammar
a few classes deep. The answer depends only on the name, the current package
and the registry, and every registry write moves the registry write
generation, so it is now kept per (generation, package, name).

YAMLish runs both on every plain scalar it concretizes, through one
`Schema::Core.new.parse` with two code blocks. One warm `load-yaml` of
`bench-yaml-parse-big`'s document, `MUTSU_JIT=off`, callgrind: 109.30M ->
107.62M instructions (-1.5%) and 62.9k -> 60.5k allocations (#12917).
