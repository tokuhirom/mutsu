Range::SetOps now passes under mutsu. This covers anonymous scalar assignments,
Capture-to-Set coercion, placeholder closure aliases, user-defined set-operator
dispatch in binary and reduction forms, and generic `minmax` ranges such as
Date ranges.
