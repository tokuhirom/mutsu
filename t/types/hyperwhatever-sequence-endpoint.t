use Test;

plan 3;

# Gauge (ecosystem) t/02-basic: `(**...**)` has HyperWhatever endpoints on the
# sequence operator; `**...` was read as a power operator.
isa-ok (**...**), Seq, 'both endpoints';
isa-ok (**...3), Seq, 'leading **';
isa-ok (1...**), Seq, 'trailing **';
