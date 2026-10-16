use nqp;
unit module NestedSubRoutineArray;

# The JSON::Fast `to-json` shape: helper subs nested in a routine share the
# routine's own `my str @out` (and an object-typed `my Str @log`).
our sub render(\obj) is export {
    my str @out;
    my Str @log;
    sub wrap(\x) {
        nqp::push_s(@out, '{');
        emit-one(x);
        nqp::push_s(@out, '}');
    }
    sub emit-one(\x) {
        nqp::push_s(@out, x);
        @log.push(x);
    }
    wrap(obj);
    nqp::join('', @out) ~ '|' ~ @log.join(',')
}
