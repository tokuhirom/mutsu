# A failed `.parse` no longer calls grammar methods twice

When `Grammar.parse` failed, mutsu re-walked the start rule to measure how far
the parse got, for the failure's position. That probe already kept embedded
code blocks and token wrappers inert (ADR-0009), but a grammar *method* reached
as a subrule — an overridden `method ws`, a `<counted>` that bumps a counter —
still ran a second time at every position the real match had visited.

The probe now treats such a call as no match instead of running it, and the
position it reports is floored at the end of the live match's longest partial
match, so the diagnostic does not get worse where the method used to let the
probe see further. Closes #11608.
