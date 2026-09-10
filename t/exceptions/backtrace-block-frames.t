use Test;

plan 18;

# A genuine bare block `{ ... }` is a Raku callframe: a backtrace captured while
# executing inside it must include an anonymous (non-routine) frame for the
# block. This mirrors roast/S32-exceptions/misc.t test 157.

# Two nested bare blocks around a failing sub -> foo, block, block, <unit> = 4.
{
    {
        my sub foo { fail }();
        CATCH { default {
            my $bt = .backtrace;
            is $bt.list.elems, 4, 'two nested bare blocks contribute two frames';
            is $bt.list[0].code.name, 'foo', 'innermost frame is the failing sub';
            nok $bt.list[1].is-routine, 'a bare-block frame is not a routine';
            is $bt.list[1].subname, '', 'a bare-block frame has an empty subname';
            is $bt.list[*-1].subname, '<unit>', 'the bottom frame is <unit>';
            is-deeply $bt.flat, $bt.list, '.flat equals .list';
        }}
    }
}

# Native `die` is implemented outside Raku source, but its logical `throw` and
# `die` setting frames still precede the real user frames. The explicit try
# source block is itself one of those user frames.
sub zipi { { { die "Something bad happened" }() }() }
try { zipi }
my $die_bt = $!.backtrace;
is $die_bt.list.elems, 7, 'die through nested blocks has the full logical frame count';
is-deeply $die_bt.list[0, 1].map(*.subname).list, <throw die>,
    'native die contributes throw and die setting frames';
is $die_bt.list[0, 1].grep(*.is-setting).elems, 2,
    'native die frames are marked as setting frames';
is $die_bt.list[2..*].grep(*.is-setting).elems, 0,
    'user frames are not marked as setting frames';
is $die_bt.list[5].subname, '', 'the explicit try block is an anonymous callframe';
is $die_bt.list[*-1].subname, '<unit>', 'the full logical trace still ends at <unit>';

# A single bare block around a failing sub -> foo, block, <unit> = 3.
{
    my sub bar { fail }();
    CATCH { default {
        is .backtrace.list.elems, 3, 'one bare block contributes one frame';
    }}
}

# An if/while body is NOT a callframe: it contributes no anonymous-block frame
# (one with an empty subname), unlike a bare block.
sub probe { die "x" }
if True {
    probe();
    CATCH { default {
        is .backtrace.list.grep({ .subname eq '' }).elems, 0,
            'an if body adds no anonymous-block frame';
    }}
}

# A bare block whose body throws past its own boundary must not leak a frame
# into a later, unrelated backtrace. Compare the frame count of an identical
# probe block with and without a preceding escaped throw: they must match.
sub leaky { { die "boom" } }
my $baseline;
{
    probe();
    CATCH { default { $baseline = .backtrace.list.elems } }
}
try { leaky() }
my $after_leak;
{
    probe();
    CATCH { default { $after_leak = .backtrace.list.elems } }
}
is $after_leak, $baseline, 'an escaped bare-block frame does not leak into a later backtrace';

# An explicit `ExceptionObject.throw` includes the setting `throw` frame at the
# top of `.backtrace.list` (hidden from the rendered text). Mirrors
# roast/S32-exceptions/misc.t test 161: a single bare block around a sub whose
# body throws gives throw, sub, block, <unit> = 4 frames.
{
    my sub baz { X::AdHoc.new.throw }();
    CATCH { default {
        my $bt = .backtrace;
        is $bt.list.elems, 4, '.throw contributes a leading setting frame';
        is $bt.list[0].subname, 'throw', 'the leading frame is the throw method';
        unlike $bt.Str, /throw/, 'the throw frame is hidden from the rendered text';
    }}
}
