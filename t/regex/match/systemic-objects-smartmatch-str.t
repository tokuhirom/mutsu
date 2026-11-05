use Test;
plan 8;

# `$*KERNEL ~~ 'linux'` is the documented way to pick a platform: Str.ACCEPTS
# compares the object's stringification, which for the Systemic objects is
# their name.
is $*KERNEL.Str, $*KERNEL.name, 'Kernel stringifies to its name';
ok $*KERNEL ~~ $*KERNEL.name, '$*KERNEL ~~ its own name';
nok $*KERNEL ~~ 'not-a-kernel', '$*KERNEL ~~ a different name is False';
ok $*PERL ~~ 'Raku', '$*PERL ~~ "Raku"';
ok $*RAKU ~~ 'Raku', '$*RAKU ~~ "Raku"';
ok $*DISTRO ~~ $*DISTRO.Str, '$*DISTRO ~~ its own stringification';
ok $*VM ~~ $*VM.Str, '$*VM ~~ its own stringification';
{
    my $hit = 0;
    given $*KERNEL {
        when $*KERNEL.name { $hit = 1 }
        default { $hit = 2 }
    }
    is $hit, 1, 'given/when picks the Kernel by name';
}
