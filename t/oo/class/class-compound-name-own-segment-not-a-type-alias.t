use lib 't/lib';
use Test;
use CompoundOwnSegmentEnum;

plan 4;

# `class Y::Errno` declared at file scope is not declared inside `Y`, so inside
# the class the bare `Errno` is the imported enum, not the class itself (#16035).
class Y::Errno {
    has Errno $.errno;
    method make($e) { self.new(errno => $e) }
    method make-bless($e) { self.bless(errno => $e) }
    method kind { Errno.^name }
}

is Y::Errno.make(EB).errno, EB, 'an attribute typed with the imported enum accepts it from a method';
is Y::Errno.make-bless(EA).errno, EA, 'bless from a method too';
is Y::Errno.new(errno => EA).errno, EA, 'construction from outside still works';
is Y::Errno.kind, 'Errno', 'the bare name inside the class is the enum';
