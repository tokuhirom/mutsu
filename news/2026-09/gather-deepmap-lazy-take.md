# Lazy gather pulls retain takes from nested callbacks

`gather @nested.deepmap: *.take` now keeps every value when the gather is
consumed through a lazy `map`. A bounded pull used to suspend at the first
`take` inside the callback, leaving the gather's coroutine with only its first
leaf. This affected `ML::AssociationRuleLearning`'s Apriori implementation,
which uses the same shape while constructing transaction tries.
