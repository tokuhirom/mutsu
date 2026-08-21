use Test;
plan 4;

# `$obj.method.attr<key> = val` — subscript assignment through a chained
# method-call result (a STACK-COMPUTED target, not a named variable) on an
# `is Hash`/`is Map` subclass instance. Found while triaging a regression in
# the bundled Template::Mustache battery (its `Logger.routines` attribute is
# exactly this shape: `LoggersMap is Hash does Associative[...]`, and
# `$m.logger.routines<Warn> = &die` silently vanished — the mutation never
# reached the shared attribute cell). `IndexAssignExprNamed`'s
# `__mutsu_hash_storage` handling (see t/hash-subclass-*.t) only covers a
# NAMED-variable target; this exercises the separate `IndexAssignGeneric`
# opcode a chained method-call target compiles to.
class LoggersMap is Hash { }
class Logger {
    has LoggersMap $.routines .= new(warn => 'note-fn');
}
class App {
    has Logger $.logger .= new;
}

my $app = App.new;
is $app.logger.routines<warn>, 'note-fn', 'initial value reads through the chain';
$app.logger.routines<warn> = 'die-fn';
is $app.logger.routines<warn>, 'die-fn', 'assignment through a chained method-call target sticks';
is $app.logger.routines.^name, 'LoggersMap', 'the class identity survives the assignment';

# The Positional twin: an `is Array` subclass instance reached the same way
# (two method-call levels deep, matching the `$app.logger.routines` shape
# above — a single-level `$obj.attr[i] = v` compiles through a different,
# separate compiler path not exercised by this fix).
class ArrSub is Array { }
class Inner {
    has ArrSub $.items .= new(1, 2, 3);
}
class Outer {
    has Inner $.inner .= new;
}
my $o = Outer.new;
$o.inner.items[1] = 99;
is $o.inner.items[1], 99, 'positional assignment through a chained method-call target sticks';
