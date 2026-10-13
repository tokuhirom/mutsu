# Match under an associative multi-dimensional subscript

`$match{*;'key'}` (and any `{a;b}` read on a Match) now walks the named
captures: the Match is treated as its named-capture hash at the first
dimension and at each nested capture, instead of as a positional capture list
that has no such keys. LLM::Prompts' grammar tests read
`PromptLine.parse(...){*;'prompt-function-cell-spec'}` this way, so its
`t/02-prompt-parsing.rakutest` now passes 9/9 (all four baseline files match
rakudo).
