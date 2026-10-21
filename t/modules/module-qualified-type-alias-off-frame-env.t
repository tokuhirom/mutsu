use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# A class a module declares with a qualified name it does not own
# (`class X::Imported::Boom` in `unit module M`, stored as
# `M::X::Imported::Boom`) is reached by the program under the written
# spelling. That alias is a package symbol of the program, not a binding of
# its frame env (ADR-0084, #7817); every way of naming the type still finds
# it. Checked against rakudo.

use ImportedExceptionType;

plan 6;

is X::Imported::Boom.^name, 'ImportedExceptionType::X::Imported::Boom', 'the bareword';
is X::Imported::Boom:U.^name, 'ImportedExceptionType::X::Imported::Boom:U', 'with a type smiley';
is ::('X::Imported::Boom').^name, 'ImportedExceptionType::X::Imported::Boom', 'an indirect lookup';
my X::Imported::Boom $typed .= new;
isa-ok $typed, X::Imported::Boom, 'as a variable constraint';
sub takes(X::Imported::Boom:D $b) { $b.message }
is takes(X::Imported::Boom.new), 'boom', 'as a parameter constraint';
try X::Imported::Boom.new.throw;
is $!.message, 'boom', 'thrown and caught';
