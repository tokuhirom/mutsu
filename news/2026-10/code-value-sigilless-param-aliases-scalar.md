# A code value's sigilless parameter aliases a scalar argument

`my $c = sub (\t) { t = 6 }; $c($s)` died with "Cannot modify an immutable Int" when `$s` lived in a local slot, because the binder looked the source up in the env, found nothing, and marked the parameter read-only. When the call already queued a writeback for the parameter, the binder now treats it as writable (copy back on exit), as the topic case did. Anonymous subs, pointy blocks and `my &b` now behave like a named sub (#14523).
