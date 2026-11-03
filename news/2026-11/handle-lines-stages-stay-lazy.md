# Stages chained on `IO::Handle.lines` stay lazy

`$fh.lines` is a lazy Seq, and so are the stages chained on it in Rakudo:
`.map`, `.grep`, `.pairs`, `.antipairs`, `.rotor`, `.batch`, `.skip`,
`.unique`, ... mutsu read the whole handle the moment one of them was called
(`$fh.tell` was the file size, a `.head(1)` read every line). An untouched
`.lines` / `.words` Seq is now piped into the stage one line at a time:

- `.map` / `.grep` chain onto the handle through the existing
  `MapGrepItems::Chain` upstream, which learned to pull from an
  `IoLines` source (`pull_map_grep_prefix_into`);
- `.pairs` / `.antipairs` / `.rotor` / `.batch` / `.skip` / ... take the
  handle as the source of the existing lazy index/adaptor pipes
  (`lazy_stream`), whose element pull now reads the handle a prefix at a
  time (`pull_source_element`).

The receiver is consumed as before (`X::Seq::Consumed` on a second use) and
full consumption yields the same values. `.kv` already had its own deferred
form and is unchanged.
