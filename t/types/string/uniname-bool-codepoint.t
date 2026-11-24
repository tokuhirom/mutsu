use Test;

# #19089: `uniname` of a Bool names its numeric codepoint, as rakudo's does
# (it coerces through Int). The routine form used to name the first
# character of the Bool's string form (`T`).

plan 7;

is uniname(True),  '<control-0001>', 'uniname(True) is codepoint 1';
is uniname(False), '<control-0000>', 'uniname(False) is codepoint 0';
is True.uniname,   '<control-0001>', 'the method form agrees';
is uniname(65),    'LATIN CAPITAL LETTER A', 'an Int is a codepoint';
is uniname("65"),  'DIGIT SIX', 'a Str names its first character';
is uniname(1.5),   'DIGIT ONE', 'a Rat names its first character';
throws-like { uniname(Str) }, X::Multi::NoMatch, 'a type object does not match';
