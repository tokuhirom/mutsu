# `>>.method` on a user class that does Iterable walks its iterator

`$obj>>.id` where `$obj`'s class composes `Iterable` called `.id` on the object itself
(`No such method 'id' for invocant of type 'Comment::ResultSeq'`). The hyper method call now
decomposes such an instance through `iterator`, exactly as `for`, list assignment and the
set operators already did, and calls the method on each element. Red `t/83-polymorphic`
(`Post.^all.Seq.map(|*.comments>>.id)`) now passes.
