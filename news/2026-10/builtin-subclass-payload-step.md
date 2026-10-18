# A built-in subclass answers its base's methods on its payload

The nineteen-name list that sent an `is Pair` subclass's methods to the `Pair` it carries is
gone (ADR-11276 §9.180). A method a built-in subclass does not declare now runs on its payload
through one step for every such subclass, `Any`'s rows no longer answer an `is Pair` or
quant-hash subclass on the instance itself (so `.Hash` is the `Pair`'s), and `.Str` of an
`is Pair` subclass is the `Pair`'s `key\tvalue`.
