use Test;

# `@a.append: $channel.list` flattens the channel's values in. A `Channel.list`
# (and a supply's lazy `.list`) is pulled on demand and has nothing cached
# until then, so `append` used to add one empty List (`[()]`), both on a plain
# array and on an outer array appended to from another thread.

plan 4;

sub chan(*@v) { my $c = Channel.new; $c.send($_) for @v; $c.close; $c }

{
    my @a;
    @a.append: chan(1, 2).list;
    is-deeply @a, [1, 2], 'append flattens a Channel.list';
}

{
    my @a = 0;
    @a.prepend: chan(1, 2).list;
    is-deeply @a, [1, 2, 0], 'prepend flattens a Channel.list';
}

{
    my @a = 0;
    @a.append: chan(1).list, 5;
    is @a.elems, 3, 'with several arguments the list stays one element';
}

{
    my @a;
    await start { @a.append: chan(3, 4).list };
    is-deeply @a, [3, 4], 'an outer array appended to from a thread';
}
