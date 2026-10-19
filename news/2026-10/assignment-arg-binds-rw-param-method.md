# An assignment passed to a method binds its `is rw` parameter

An assignment evaluates to its target's container. A sub call already used
this to bind an `is rw` parameter to the assigned variable. A method call
received only the value and died with "expects a writable container"
(#13934):

```raku
class K { method inc($x is rw) { $x += 10 } }
my $o = 10; K.inc(($o -= 2)); say $o;   # 18 (was an X::Parameter::RW error)
```

A method call's arguments carry no `WrapVarRef` tag. Its `is rw` binding
finds the caller's variable through the call site's recorded argument
sources. That record now names an assignment's target, as it names a plain
variable. Intl::CLDR's currency loader passes `($offset -= 2)` this way.
