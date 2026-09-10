use Test;

plan 14;

# raku decontainerizes an object hash's key before storing it, so `.keys` /
# `.kv` / `.pairs` / `.raku` hand back the bare value. mutsu kept the
# `Scalar` wrapper the subscript paths use as the transport.

{
    my %j{List:D};
    my $t = $(1, 2);
    %j{$t} = "x";
    is %j.keys[0].raku, '(1, 2)', 'the stored key is not itemized';
    is %j.keys[0].^name, 'List', '... and is still a List';
    is %j.elems, 1, '... and is one entry';
    is %j{$t}, 'x', '... which still reads back through the itemized subscript';
}

{
    my %h{Any};
    my $k = $(1, 2);
    %h{$k} = 5;
    is %h.raku, '(my Any %{Any} = (1, 2) => 5)', '.raku shows the bare key';
    is %h.kv.raku, '((1, 2), 5).Seq', '.kv too';
    is %h.pairs.raku, '((1, 2) => 5,).Seq', '.pairs too';
    is %h.gist, '{(1 2) => 5}', '.gist is unchanged';
}

# An itemized Array and an itemized Hash key get the same treatment.
{
    my %a{Any};
    my $ar = $[1, 2];
    %a{$ar} = 7;
    is %a.keys[0].raku, '[1, 2]', 'an itemized Array key is stored bare';
    is %a{$ar}, 7, '... and reads back';
}
{
    my %m{Any};
    my $hh = ${a => 1};
    %m{$hh} = 8;
    is %m.keys[0].raku, '{:a(1)}', 'an itemized Hash key is stored bare';
    is %m{$hh}, 8, '... and reads back';
}

# The de-itemized key must not change the `.WHICH` the entry is filed under,
# so every accessor still agrees on one entry.
{
    my %h{Any};
    my $k = $(1, 2);
    %h{$k} = 5;
    ok (%h{$k}:exists), ':exists agrees';
    %h{$k}:delete;
    is %h.elems, 0, ':delete agrees';
}
