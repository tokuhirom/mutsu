use Test;

plan 4;

is +^ 0, -1, '+^ accepts optional whitespace before operand';
is +^ 255, -256, '+^ with whitespace preserves integer bitwise negation semantics';
is ?^ True, False, '?^ accepts optional whitespace before operand';
is ?^ False, True, '?^ with whitespace preserves boolean bitwise negation semantics';
