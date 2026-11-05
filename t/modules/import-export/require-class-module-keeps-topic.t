use Test;

# #15959: the mainline of a module loaded by `require` / `need` runs in the
# importer's env, and its tail value was delivered through `$_`, replacing the
# `for` / `given` topic of the scope that asked for the load (a class
# declaration's value is the type object, `1;` is 1, a role's is Any).

plan 12;

use lib 't/lib';

for <a> {
    require RequireTopicMod;
    is $_, 'a', 'for topic after require of a class module';
}

for <b> {
    is $_, 'b', 'topic before the require';
    require RequireTopicMod;
    is $_, 'b', 'topic after the require, same module loaded twice';
}

for <c d> {
    require RequireTopicTail;
    is $_.Str, $_.Str, 'every iteration';
}

my @seen;
for <e f> {
    require RequireTopicTail;
    @seen.push($_);
}
is-deeply @seen, ['e', 'f'], 'a module whose last statement is 1; keeps the topic';

for <g> {
    require RequireTopicRole;
    is $_, 'g', 'a role module keeps the topic';
}

given 'h' {
    require RequireTopicMod;
    is $_, 'h', 'given topic';
}

my %m = <RequireTopicMod>.map: {
    do if $_ ~~ Str { require ::($_); $_ => ::($_) } else { .^name => $_ }
};
is %m.keys.sort.join(','), 'RequireTopicMod', 'a do-if with a symbolic require (the Red schema shape): key';
is %m<RequireTopicMod>.^name, 'RequireTopicMod', 'and the value is the loaded type';

for <i> -> $v {
    require RequireTopicMod;
    is $v, 'i', 'an explicit parameter is unaffected';
}

for <j> {
    my $x = (require RequireTopicMod);
    is $_, 'j', 'require as an expression is unaffected';
}
