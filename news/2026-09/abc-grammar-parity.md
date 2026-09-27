# ABC grammar tests reach parity

ABC 0.6.13 now passes all ten baseline test files under mutsu, covering 904
assertions. Regex alternation captures now retain their statically determined
list shape even when an earlier branch supplies the selected capture, and
WhateverCode smartmatchers now receive Pair topics so grammar actions can
classify captured pairs with method calls such as `*.key`.

The ecosystem ledger now records ABC as green with no regressed files.
