use Test;

# Unicode operator aliases (×, ÷, −, ≤, ≥, ≠) work inside hyper meta-ops, just
# as they do as plain infixes. Previously a hyper op with a Unicode inner
# operator failed with "Unsupported reduction operator".

plan 14;

# --- plain infixes (already worked; guard against regression) ---
is 2 × 3, 6, '× as a plain infix';
is 8 ÷ 2, 4, '÷ as a plain infix';
is 5 − 3, 2, '− as a plain infix';
ok 3 ≤ 3, '≤ as a plain infix';
ok 5 ≥ 3, '≥ as a plain infix';
ok 3 ≠ 4, '≠ as a plain infix';

# --- inside hyper meta-ops ---
is-deeply ([1,2,3] »×» [4,5,6]), [4, 10, 18], '× inside a hyper op';
is ([6,8] »÷» [2,4]).join(','), '3,2', '÷ inside a hyper op';
is-deeply ([10,20] »−» [1,2]), [9, 18], '− inside a hyper op';
is-deeply ([1,2,3] »≤» [2,2,2]), [True, True, False], '≤ inside a hyper op';
is-deeply ([1,5,3] »≥» [2,2,2]), [False, True, True], '≥ inside a hyper op';
is-deeply ([1,2,3] »≠» [1,5,3]), [False, True, False], '≠ inside a hyper op';

# --- one-sided hyper with a scalar ---
is-deeply ((1,2,3) »×» 10), (10, 20, 30), '× hyper against a scalar';
is ([2,4,6] >>÷>> 2).join(','), '1,2,3', 'ASCII >>op>> form with ÷';
