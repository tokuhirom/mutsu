# RakuAST: origins of a hyper method call

`@a>>.name` and `@a».name` now have rakudo's ranges: the `MetaPostfix::Hyper`
from the `>>` to the end, and the `Call::Method` it holds from the method's
name, with its `Name` and `ArgList` (#15780). It was the largest remaining gap
of `rakuast-construct-return-type.t`.
