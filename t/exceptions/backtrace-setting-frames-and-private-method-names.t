use Test;

# #19244: the leading `throw` / `die` frames of a backtrace live in the CORE
# setting, and a private method's frame carries its public name.
plan 12;

sub frames($e) { $e.backtrace.list }

try { die "x"; CATCH { default {
    my @f = frames($_);
    is @f[0].subname, 'throw', 'the first frame is throw';
    is @f[0].file, 'SETTING::src/core.c/Exception.rakumod', 'in the setting';
    is @f[0].line, 65, 'at rakudo 2026.11\'s line';
    ok @f[0].is-setting, 'a setting frame';
    is @f[1].subname, 'die', 'the second frame is die';
    is @f[1].file, 'SETTING::src/core.c/control.rakumod', 'in the setting';
    is @f[1].line, 253, 'at its line';
    ok @f[2].file.ends-with('.t'), 'the user frame keeps its own file';
} } }

try { X::AdHoc.new(payload => "z").throw; CATCH { default {
    my @f = frames($_);
    is-deeply @f[0].file, 'SETTING::src/core.c/Exception.rakumod', 'an explicit .throw has the throw frame only';
} } }

class C {
    method !p { die "x" }
    method q { self!p }
}
try { C.q; CATCH { default {
    is-deeply frames($_).map(*.subname).List, ('throw', 'die', 'p', 'q', '', '<unit>'),
        'a private method frame is named without the !';
    like .backtrace.Str, /'in method p '/, 'and so is its rendered line';
    unlike .backtrace.Str, /'method !p'/, 'with no private marker';
} } }
