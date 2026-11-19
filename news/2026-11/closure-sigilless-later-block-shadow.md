# A closure keeps reading an outer `\x` that a later block shadows

```raku
my \inner = "outer";
sub c() { my $o = { inner }; say $o(); { my \inner = "block"; say $o() } }
c();   # was Nil / Nil, now outer / outer as in Rakudo
```

The closure's free `\inner` was read, at closure creation, from the routine's
slot of that name, which belonged to the nested block's later `my \inner` and
was still unset. The compiler records, per captured name, the creating frame's
slot at the closure's creation point; a `None` there now makes the capture
fall back to the name's binding for a sigilless term too, as it already did for
a `for` loop parameter (#18176).
