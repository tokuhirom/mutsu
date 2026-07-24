unit class CrlfBase;
# A module-level scalar `my` whose bare name (mutsu strips the `$`) collides with
# a same-named `my` in a child module and a `constant` elsewhere.
my $CRLF = "base-crlf";
method base-crlf() { $CRLF }
