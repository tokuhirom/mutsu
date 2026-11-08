# A rule reached through a grammar method fires its own action

A grammar method that delegates to a rule (`method key($n) { self."$n"() }`,
called as `<x=.key('thing')>` or `<.key('thing')>`) returns that rule's Match.
The regex atom that called the method now files the capture under the delegate
rule's action name and keeps the delegate's sub-captures, so `thing`'s action
fires with `$<k>`/`$<v>` intact, as in rakudo. A stale delegate record is cleared
before each method call so an unrelated method cannot borrow another rule's action.
This is the shape of the slang host's `key-origin('rule', 'keyword')`
(#13370, #16888).
