# A subset's value predicate closes over the declaring file's lexicals

`subset Hyperable of Str where .starts-with($header)` was closed over its
declaring scope only when the predicate was written as a code literal or named
a type. A value predicate reading a file-scope `my constant` was checked in the
caller's scope instead, where `$header` is not visible, so a family exported
from the module never chose its `Hyperable` candidate (`JSON::Fast::Hyper`'s
`from-json`, #14500). Any predicate with a free lexical is now closed over.
