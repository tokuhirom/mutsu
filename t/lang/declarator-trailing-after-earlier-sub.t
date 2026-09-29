use Test;

plan 2;

# A trailing `#=` after a multi-line sub must attach to that sub even when an
# earlier one-line sub (`sub x {}`) opened and closed its body on one line.
sub x {}
#| lead
sub cast($s) {
  $s;
}
#= trail
is &cast.WHY.Str, "lead\ntrail", 'leading and trailing doc attach to the multi-line sub';
is &x.WHY.defined, False, 'the earlier one-line sub has no doc';
