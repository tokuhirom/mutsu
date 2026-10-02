use Test;

# Found via the P5caller distribution: a `Backtrace::Frame`'s `.code` is a
# `Sub` that knows its declaring package, a bareword call of a routine
# declared later records its own call-site line, and a user-declared `caller`
# sub takes over from mutsu's native extension.

plan 7;

module M {
    our sub inner is export {
        my $bt = Backtrace.new;
        my $i = $bt.next-interesting-index(-1);
        $bt[$i].code;
    }
}
import M;

sub foo { inner }
my $code = foo();
isa-ok $code, Sub, 'frame code of a module sub is a Sub';
is $code.package.^name, 'M', 'frame code reports the declaring package';
is $code.name, 'inner', 'frame code reports the name';

my $bare-line = $?LINE + 1;
sub outer-bare { bare-later }
sub bare-later {
    Backtrace.new.list.map({ .subname => .line }).grep(*.key eq 'outer-bare').head.value
}
sub call-it { outer-bare }
is call-it(), $bare-line, 'bareword call-site line is the line of the bareword';

use lib 't/lib';
use UserCallerSub;
is caller, 'mine', 'an imported caller replaces the native one (bare)';
is caller(1), 'mine1', 'an imported caller replaces the native one (call)';
is caller().^name, 'Str', 'the result is the user routine\'s';
