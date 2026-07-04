use Test;

plan 9;

# `.uniprop('NFC_Quick_Check')` (and the NFD/NFKC/NFKD variants) return the full
# property-value names Yes/No/Maybe using the authoritative NFC_QC data — so a
# character that can compose with a following one is `Maybe`, not `Yes`. MoarVM
# returns the short Y/N/M codes and does not compute Maybe (a `#?rakudo.moar
# todo` in roast S15-unicode-information/uniprop.t); mutsu is more correct.

# NFC_Quick_Check: Yes / No / Maybe.
is '都'.uniprop('NFC_Quick_Check'),     'Yes',   'a normal ideograph is NFC_QC=Yes';
is 0x0374.uniprop('NFC_Quick_Check'),   'No',    'U+0374 is NFC_QC=No';
is "\x[11A8]".uniprop('NFC_Quick_Check'), 'Maybe', 'a Hangul trailing consonant is NFC_QC=Maybe';

# The short alias resolves the same.
is '都'.uniprop('NFC_QC'), 'Yes', 'the NFC_QC short alias works';

# NFD_Quick_Check is Yes/No only.
is 'A'.uniprop('NFD_Quick_Check'),      'Yes', 'ASCII "A" is NFD_QC=Yes';
is "\x[00C0]".uniprop('NFD_Quick_Check'), 'No', 'À (precomposed) is NFD_QC=No';

# NFKC / NFKD quick-check.
is '①'.uniprop('NFKC_Quick_Check'),     'No',  '① is NFKC_QC=No (has a compatibility decomposition)';
is '①'.uniprop('NFKD_Quick_Check'),     'No',  '① is NFKD_QC=No';
is 'a'.uniprop('NFKC_Quick_Check'),     'Yes', 'plain ASCII is NFKC_QC=Yes';
