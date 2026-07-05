use Test;

# Joining_Group property (ArabicShaping.txt). mutsu previously derived the
# value from the character name, which returned the full letter name instead
# of the shaping group (e.g. "ALEF WITH MADDA ABOVE" instead of "ALEF").

plan 16;

# Arabic letters sharing a group
is "\x0627".uniprop('Joining_Group'), 'ALEF', 'alef is ALEF';
is "\x0622".uniprop('Joining_Group'), 'ALEF', 'alef with madda is ALEF';
is "\x0625".uniprop('Joining_Group'), 'ALEF', 'alef with hamza below is ALEF';
is "\x0628".uniprop('Joining_Group'), 'BEH', 'beh is BEH';
is "\x062A".uniprop('Joining_Group'), 'BEH', 'teh is in group BEH';
is "\x062B".uniprop('Joining_Group'), 'BEH', 'theh is in group BEH';
is "\x062C".uniprop('Joining_Group'), 'HAH', 'jeem is in group HAH';
is "\x0629".uniprop('Joining_Group'), 'TEH MARBUTA', 'teh marbuta group';
is "\x0647".uniprop('Joining_Group'), 'HEH', 'heh is HEH';

# Syriac
is "\x0710".uniprop('Joining_Group'), 'ALAPH', 'Syriac alaph is ALAPH';
is "\x0712".uniprop('Joining_Group'), 'BETH', 'Syriac beth is BETH';
is "\x0718".uniprop('Joining_Group'), 'SYRIAC WAW', 'Syriac waw group';

# Extended / other scripts
is "\x08AC".uniprop('Joining_Group'), 'ROHINGYA YEH', 'Rohingya yeh group';
is "\x10AC0".uniprop('Joining_Group'), 'MANICHAEAN ALEPH', 'Manichaean aleph group';

# Non-joining characters
is 'A'.uniprop('Joining_Group'), 'No_Joining_Group', 'Latin letter has no group';
is "\x064B".uniprop('Joining_Group'), 'No_Joining_Group', 'Arabic fathatan has no group';
