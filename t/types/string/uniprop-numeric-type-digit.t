use Test;

# Numeric_Type=Digit covers single-digit (0..9) presentation forms that are
# not decimal digits (gc=Nd): superscripts/subscripts and the circled,
# parenthesized, full-stop and dingbat digit forms. mutsu previously only
# recognised the superscript/subscript subset and reported the rest as
# Numeric.

plan 22;

# Superscripts / subscripts (already worked)
is '²'.uniprop('Numeric_Type'), 'Digit', 'superscript two is Digit';
is '⁷'.uniprop('Numeric_Type'), 'Digit', 'superscript seven is Digit';
is '₃'.uniprop('Numeric_Type'), 'Digit', 'subscript three is Digit';

# Circled digits ①..⑨ and ⓪
is '①'.uniprop('Numeric_Type'), 'Digit', 'circled one is Digit';
is '⑨'.uniprop('Numeric_Type'), 'Digit', 'circled nine is Digit';
is '⓪'.uniprop('Numeric_Type'), 'Digit', 'circled zero is Digit';

# Parenthesized digits ⑴..⑼
is '⑴'.uniprop('Numeric_Type'), 'Digit', 'parenthesized one is Digit';
is '⑼'.uniprop('Numeric_Type'), 'Digit', 'parenthesized nine is Digit';

# Digit-with-full-stop ⒈..⒐
is '⒈'.uniprop('Numeric_Type'), 'Digit', 'digit one full stop is Digit';

# Double-circled ⓵..⓽ and ⓿
is "\x24F5".uniprop('Numeric_Type'), 'Digit', 'double circled one is Digit';
is "\x24FF".uniprop('Numeric_Type'), 'Digit', 'negative circled zero is Digit';

# Dingbat circled/negative digits
is "\x2776".uniprop('Numeric_Type'), 'Digit', 'dingbat negative circled one is Digit';
is "\x2780".uniprop('Numeric_Type'), 'Digit', 'dingbat circled sans-serif one is Digit';

# Ethiopic digits
is "\x1369".uniprop('Numeric_Type'), 'Digit', 'ethiopic digit one is Digit';

# Numeric_Value is still correct for these
is '①'.unival, 1, '① unival is 1';
is '⓪'.unival, 0, '⓪ unival is 0';

# Non-Digit numeric forms stay Numeric
is '½'.uniprop('Numeric_Type'), 'Numeric', 'vulgar fraction is Numeric';
is '⑩'.uniprop('Numeric_Type'), 'Numeric', 'circled ten is Numeric';
is 'Ⅴ'.uniprop('Numeric_Type'), 'Numeric', 'Roman numeral is Numeric';

# Plain decimal digits and letters unchanged
is '7'.uniprop('Numeric_Type'), 'Decimal', 'ASCII digit is Decimal';
is '٣'.uniprop('Numeric_Type'), 'Decimal', 'Arabic-Indic digit is Decimal';
is 'A'.uniprop('Numeric_Type'), 'None', 'letter is None';
