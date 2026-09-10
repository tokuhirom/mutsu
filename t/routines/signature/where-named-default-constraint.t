use Test;

plan 12;

# A `where` constraint on an *unsupplied* optional named parameter must be
# checked against its default / type-object value -- not silently skipped.
# Raku checks `:v($) where .so` even when `--v` is absent: the topic is then
# the `Bool` type object (`.so` is False), so the candidate is rejected.

multi sub cmd(Bool :version($) where .so) { 'version' }
multi sub cmd()                            { 'default' }

is cmd(),               'default', 'where .so on absent named -> falls through to ()';
is cmd(:version),       'version', 'where .so on present truthy named -> matches';
is cmd(version => True),'version', 'explicit True named -> matches';
# `version => False` fails the `where .so` candidate, and the `()` candidate
# rejects an extra named arg, so no candidate resolves (Raku raises).
dies-ok { cmd(version => False) }, 'explicit False named -> no candidate resolves';

# Direct (non-multi) call: the where on an unsupplied named with a type object
# default must fail the binding (X::TypeCheck::Binding::Parameter), like Raku.
sub need-so(Bool :flag($) where .so) { 'ok' }
is need-so(:flag), 'ok', 'direct: truthy named satisfies where';
throws-like { need-so() }, X::TypeCheck::Binding::Parameter,
    'direct: unsupplied named fails where against type object';

# A supplied value that fails the where still raises the typed binding error.
sub pos-n(Int :$n where * > 3) { $n }
is pos-n(n => 5), 5, 'supplied named passing where';
throws-like { pos-n(n => 2) }, X::TypeCheck::Binding::Parameter,
    'supplied named failing where raises typed exception';

# A named param with an explicit default whose default fails the where is
# rejected when the arg is omitted.
sub def-fail(:$x where * > 10 = 3) { $x }
throws-like { def-fail() }, X::TypeCheck::Binding::Parameter,
    'default value failing where is rejected';
sub def-ok(:$x where * > 10 = 42) { $x }
is def-ok(), 42, 'default value satisfying where is accepted';
is def-ok(x => 99), 99, 'supplied value overriding satisfying default';

# Block-form where on an unsupplied named (topic is the type object).
multi sub blk(Int :n($) where { .defined && $_ > 0 }) { 'pos' }
multi sub blk()                                       { 'none' }
is blk(), 'none', 'block where on absent named falls through';
