use Test;

# `try_proto_method_body`'s interception gate required `ValueView::Instance`,
# so a proto method's non-trivial body never ran for a TYPE-OBJECT invocant
# (P.m(5), not P.new.m(5)) -- it fell through straight to ordinary multi
# resolution instead. Verified against raku directly.

plan 4;

my @log;

class P {
    proto method m($x) { @log.push("proto($x)"); {*} }
    multi method m(Int $x) { @log.push("int($x)") }
}
P.m(5);
is @log.join('/'), 'proto(5)/int(5)', 'proto body runs for a type-object invocant';

@log = ();
class Q is P {
    multi method m(Str $s) { @log.push("str($s)") }
}
Q.m("a");
is @log.join('/'), 'proto(a)/str(a)', 'inherited proto body runs for a subclass type-object invocant';

class R {
    proto method g(Int $n) {
        return 'suppressed' if $n < 0;
        {*}
    }
    multi method g(Int $n) { "ok:$n" }
}
is R.g(5), 'ok:5', 'proto body return value flows through for a type-object invocant';
is R.g(-1), 'suppressed', 'proto body can short-circuit before {*} for a type-object invocant';
