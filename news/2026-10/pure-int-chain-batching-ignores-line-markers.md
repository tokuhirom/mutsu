# Pure Int grep/map chain batching sees through closure line markers

#14164 gave every closure body a `Stmt::SetLine` marker, so the one-statement check in
`pure_int_topic_expression` (`src/vm/vm_map_grep_pure.rs`) stopped matching and
`@a.grep({ $_ %% 3 }).map({ $_ * 2 })` fell back to one pull per element. The check now
counts only real statements. See #14286 for the before/after numbers.
