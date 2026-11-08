# `.origin` of an assigning subscript (`%h<k> = v`, `@a[i] = v`)

The subscript node of `%e<M> = 1` / `@a[0] = 1` carried no origin, and neither did its
index (`QuotedString`/`StrLiteral`, or the `SemiList` and its statement). Rakudo's subscript
runs through the assigned value, while its brackets and key keep their own ranges; the origin
pass now scans the brackets and adverbs up to the `=` and gives the subscript the full range
(#15780).
