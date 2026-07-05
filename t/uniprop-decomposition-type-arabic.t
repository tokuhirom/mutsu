use Test;

# Decomposition_Type of Arabic presentation forms encodes the positional
# form (Isolated / Final / Initial / Medial). mutsu previously reported
# every character in these blocks as Isolated.

plan 14;

# Arabic Presentation Forms-B (FE70-FEFF): BEH forms
is "\xFE8F".uniprop('Decomposition_Type'), 'Isolated', 'BEH isolated is Isolated';
is "\xFE90".uniprop('Decomposition_Type'), 'Final', 'BEH final is Final';
is "\xFE91".uniprop('Decomposition_Type'), 'Initial', 'BEH initial is Initial';
is "\xFE92".uniprop('Decomposition_Type'), 'Medial', 'BEH medial is Medial';

# ALEF (only isolated/final)
is "\xFE8D".uniprop('Decomposition_Type'), 'Isolated', 'ALEF isolated is Isolated';
is "\xFE8E".uniprop('Decomposition_Type'), 'Final', 'ALEF final is Final';

# LAM (four forms)
is "\xFEDD".uniprop('Decomposition_Type'), 'Isolated', 'LAM isolated is Isolated';
is "\xFEDE".uniprop('Decomposition_Type'), 'Final', 'LAM final is Final';
is "\xFEDF".uniprop('Decomposition_Type'), 'Initial', 'LAM initial is Initial';
is "\xFEE0".uniprop('Decomposition_Type'), 'Medial', 'LAM medial is Medial';

# Arabic Presentation Forms-A (FB50-FDFF): ligatures are Isolated
is "\xFB50".uniprop('Decomposition_Type'), 'Isolated', 'ALEF WASLA isolated is Isolated';
is "\xFDF2".uniprop('Decomposition_Type'), 'Isolated', 'ALLAH ligature is Isolated';

# A form with an initial variant in Forms-A
is "\xFB54".uniprop('Decomposition_Type'), 'Initial', 'BEEH initial is Initial';
is "\xFB55".uniprop('Decomposition_Type'), 'Medial', 'BEEH medial is Medial';
