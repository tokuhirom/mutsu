use Test;

# The `:bin` chunks a Proc::Async delivers are `Buf[uint8]`, not plain `Buf`
# (#16150). Checked against rakudo.

plan 6;

{
    my $p = Proc::Async.new('echo', 'hi');
    my @chunks;
    $p.stdout(:bin).tap: -> $x { @chunks.push: $x };
    await $p.start;
    is @chunks[0].^name, 'Buf[uint8]', 'stdout(:bin): the chunk type name';
    is @chunks[0].raku, 'Buf[uint8].new(104,105,10)', '...and its .raku';
    is @chunks[0].decode.chomp, 'hi', '...still decodes';
}

{
    my $p = Proc::Async.new('echo', 'hi');
    my @chunks;
    $p.Supply(:bin).tap: -> $x { @chunks.push: $x };
    await $p.start;
    is @chunks[0].^name, 'Buf[uint8]', 'Supply(:bin): the chunk type name';
    is @chunks[0].raku, 'Buf[uint8].new(104,105,10)', '...and its .raku';
    ok @chunks[0] ~~ Buf, '...which is still a Buf';
}
