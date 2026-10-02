# Closing one tap of a shared supply block no longer silences the others

A `.share`d `supply { whenever ... }` block runs once and every tap joins that
run, but the first tap -- the one that started the block -- carried the
block's whole teardown (its `whenever` subscriptions, CLOSE phasers and
act-loop workers) on its own `Tap` handle. Closing that tap, directly or
through a derived `map`/`grep`/`head` tap, closed the block and every other
consumer stopped receiving values.

The shared block now belongs to the share rather than to its starting tap, as
in raku, whose `Supply.share` taps the source once and never closes it:
closing a consumer drops only that consumer's subscription on the shared
supplier. (#10831)
