# `.raku` of a Seq whose source dies raises the exception

`.raku` of a deferred Seq pulls its source to render the elements, and swallowed whatever that
pull died with: `(1, 2).map({ die "boom" }).raku` answered the `Seq.new()` placeholder where
rakudo dies with `boom`. Only the `X::Seq::Consumed` of an already spent Seq is that
placeholder now (rakudo renders it for a consumed Seq and does not throw); any other exception
reaches the caller, as it already did for `.gist`.

What the Seq does on the reads that FOLLOW a failed one is still wrong: mutsu leaves it consumed,
while rakudo keeps the partially reified list and resumes after the failing element. The
measurement and the shape of the fix are recorded in an amendment to ADR-0034 and in #12048.
