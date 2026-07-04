use Test;

# CJK numeral ideographs carry a numeric value and therefore have
# Numeric_Type=Numeric, even though their General_Category is Lo (not a
# Number category). mutsu previously reported Numeric_Type=None for them
# while still returning the correct Numeric_Value / .unival.

plan 20;

# 一二三四五六七八九十
is '一'.uniprop('Numeric_Type'), 'Numeric', '一 is Numeric_Type=Numeric';
is '二'.uniprop('Numeric_Type'), 'Numeric', '二 is Numeric';
is '三'.uniprop('Numeric_Type'), 'Numeric', '三 is Numeric';
is '五'.uniprop('Numeric_Type'), 'Numeric', '五 is Numeric';
is '九'.uniprop('Numeric_Type'), 'Numeric', '九 is Numeric';
is '十'.uniprop('Numeric_Type'), 'Numeric', '十 is Numeric';
is '百'.uniprop('Numeric_Type'), 'Numeric', '百 is Numeric';
is '千'.uniprop('Numeric_Type'), 'Numeric', '千 is Numeric';
is '万'.uniprop('Numeric_Type'), 'Numeric', '万 is Numeric';
is '億'.uniprop('Numeric_Type'), 'Numeric', '億 is Numeric';

# Numeric_Type stays consistent with the numeric value
is '一'.uniprop('Numeric_Value'), '1', '一 Numeric_Value is 1';
is '十'.uniprop('Numeric_Value'), '10', '十 Numeric_Value is 10';
is '億'.unival, 100000000, '億 unival is 100000000';

# Existing categories still classify correctly
is '5'.uniprop('Numeric_Type'), 'Decimal', 'ASCII digit is Decimal';
is '½'.uniprop('Numeric_Type'), 'Numeric', 'vulgar fraction is Numeric';
is 'Ⅴ'.uniprop('Numeric_Type'), 'Numeric', 'Roman numeral (Nl) is Numeric';
is '²'.uniprop('Numeric_Type'), 'Digit', 'superscript two is Digit';

# Non-numeric characters are None
is 'A'.uniprop('Numeric_Type'), 'None', 'letter is None';
is '愛'.uniprop('Numeric_Type'), 'None', 'non-numeric ideograph is None';
is '〇'.uniprop('Numeric_Type'), 'Numeric', 'ideographic zero is Numeric';
