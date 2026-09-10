use Test;

# Joining_Type property (ArabicShaping.txt). mutsu previously used a
# General_Category / script heuristic that tagged every Arabic letter as D
# and every combining mark as T; verify the exact classification.

plan 18;

# Dual_Joining (D)
is "\x0628".uniprop('Joining_Type'), 'D', 'Arabic beh is D';
is "\x0645".uniprop('Joining_Type'), 'D', 'Arabic meem is D';
is "\x0712".uniprop('Joining_Type'), 'D', 'Syriac beth is D';
is "\x1E900".uniprop('Joining_Type'), 'D', 'Adlam alif is D';

# Right_Joining (R)
is "\x0627".uniprop('Joining_Type'), 'R', 'Arabic alef is R';
is "\x062F".uniprop('Joining_Type'), 'R', 'Arabic dal is R';
is "\x0648".uniprop('Joining_Type'), 'R', 'Arabic waw is R';
is "\x0710".uniprop('Joining_Type'), 'R', 'Syriac alaph is R';

# Left_Joining (L)
is "\xA872".uniprop('Joining_Type'), 'L', 'Phags-pa head letter is L';
is "\x10D00".uniprop('Joining_Type'), 'L', 'Hanifi Rohingya A is L';

# Join_Causing (C)
is "\x0640".uniprop('Joining_Type'), 'C', 'Arabic tatweel is C';
is "\x200D".uniprop('Joining_Type'), 'C', 'ZWJ is C';

# Transparent (T)
is "\x070F".uniprop('Joining_Type'), 'T', 'Syriac abbreviation mark is T';

# Non_Joining (U) — the default, including most combining marks and hamza
is "\x0621".uniprop('Joining_Type'), 'U', 'Arabic hamza is U';
is "\x064B".uniprop('Joining_Type'), 'U', 'Arabic fathatan is U';
is "\x0301".uniprop('Joining_Type'), 'U', 'combining acute is U';
is 'A'.uniprop('Joining_Type'), 'U', 'Latin letter is U';
is "\x4E00".uniprop('Joining_Type'), 'U', 'CJK ideograph is U';
