# The topic `.=` is a typed expression node

`$_ .= meth` and `.=meth` build `Expr::TopicDotAssign` instead of a call to the `__mutsu_topic_dotassign`
marker function. The compiler, the topic-mutation analysis and the visitors match the node; the
name comparison against a string is gone.
