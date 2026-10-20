# `@hashes>>.<a b c>` slices each element

A multi-word angle subscript after a hyper dot (`@d>>.<name glob regex>`) was compiled to a
single `AT-KEY` lookup with the word list as one key, so every element answered `Any`. It now
slices each element like `.{'name','glob','regex'}`. Found through the Glob::Grammar distribution,
whose test file now passes.
