use Test;

plan 14;

# Backtrace::Frame methods and Backtrace list-iteration.

{
    my sub foo { fail }();
    CATCH { default {
        my $bt = .backtrace;
        isa-ok $bt.list[0], Backtrace::Frame, '.list yields Backtrace::Frame objects';
        is $bt.list[0].code.name, 'foo', '.code.name returns the routine name';
        ok $bt.list[0].is-routine, 'named frame .is-routine is True';
        nok $bt.list[0].is-hidden, '.is-hidden is False';
        nok $bt.list[0].is-setting, '.is-setting is False';
        is-deeply $bt.flat, $bt.list, '.flat returns the same as .list';
    }}
}

{
    my sub bar { die }();
    CATCH { default {
        my $bt = .backtrace;
        # .concise == grep(routine, non-hidden, non-setting).join
        is $bt.concise,
            $bt.grep({ !.is-hidden && .is-routine && !.is-setting }).join,
            '.concise matches the documented grep';
        is $bt.summary,
            $bt.grep({ !.is-hidden && (.is-routine || !.is-setting) }).join,
            '.summary matches the documented grep';
        ok $bt.list.elems >= 1, 'die backtrace has at least one frame';
    }}
}

# A thrown exception object carries a non-empty backtrace.
{
    my sub baz { X::AdHoc.new(payload => "boom").throw }();
    CATCH { default {
        my $bt = .backtrace;
        ok $bt.list.elems >= 1, 'thrown exception has a backtrace';
        ok $bt.list.all ~~ Backtrace::Frame, 'thrown backtrace frames are Backtrace::Frame';
        ok $bt.list.grep(*.is-routine).elems >= 1, 'thrown backtrace has a routine frame';
    }}
}

# <unit> frame: not a routine, code.name is <unit>.
{
    my sub qux { die }();
    CATCH { default {
        my $bt = .backtrace;
        my @units = $bt.list.grep({ .subname eq '<unit>' });
        ok @units.elems >= 1, 'a <unit> frame exists';
        nok @units[0].is-routine, '<unit> frame is not a routine';
    }}
}
