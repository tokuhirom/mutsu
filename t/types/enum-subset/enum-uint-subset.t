use Test;

plan 6;

enum ErrorCode <NO_ERROR PROTOCOL_ERROR INTERNAL_ERROR>;
class RstStream { has UInt $.error-code is required; }

ok INTERNAL_ERROR ~~ UInt, 'Int-based enum value matches UInt';

lives-ok { my UInt $u = INTERNAL_ERROR }, 'enum value binds to a UInt-typed variable';

lives-ok { RstStream.new(error-code => INTERNAL_ERROR) },
    'enum value satisfies a UInt attribute type check';

ok True ~~ UInt, 'Int-based Bool value matches UInt';

lives-ok { my UInt $u = True }, 'Bool value binds to a UInt-typed variable';

sub accepts-uint(UInt:D $value) { $value }
lives-ok { accepts-uint(True) }, 'Bool value satisfies a UInt:D parameter type check';

done-testing;
