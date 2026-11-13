# A lazy `.map` from a module reads the module's own file-scope lexicals

A `.map`/`.grep` Seq a module routine returns runs its callback only when the
importing script consumes it. The inline map loop pushes no frame for the
callback, so a read of the module's file-scope lexical (`my constant
%default-keys`, `my %h`) went through the consuming script's frame and found
nothing (or the script's own same-named variable). The callback now runs
with its declaring compunit marked as the not-yet-framed scope (the same
marker a multi candidate's `where` uses), and a closure it mints -- the
inner `.map` of a `.map` -- records that compunit's file rather than the
consumer's.

`Text::T9`'s `t/basic.rakutest` now passes 4/4, matching raku.
