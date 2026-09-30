use Test;

# Found via App::RakuCron (through Lumberjack::Logger): a `my Level $x`
# statement in a role body, where `enum Level` lives in the class enclosing the
# role, must resolve the short type name through the role's lexical package
# when the role is composed into a class.

plan 2;

class L {
    enum Level <Off Fatal All>;
    role Logger {
        my Level $level = L::All;
        method lvl( --> Level ) is rw { $level }
    }
}
class C does L::Logger { }

is C.new.lvl, L::All, 'typed my in role body sees enum of the enclosing class';
ok C.new.lvl ~~ L::Level, 'value is a Level';
