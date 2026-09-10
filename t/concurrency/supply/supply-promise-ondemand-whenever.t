use Test;

plan 2;

# Awaiting a supply whose `whenever` source is itself a STORED on-demand
# supply must replay that source's values. `replay_static_whenever_promise`
# read only the source's static `values` attribute — an on-demand source keeps
# its values behind `on_demand_callback`, so the body replayed zero values and
# the LAST phaser emitted the untouched accumulator
# (Cro::MessageWithBody.body-blob awaited an empty Buf).

my $src = supply {
    emit Blob.new(1, 2);
    emit Blob.new(3);
};
my $joined-result = await supply {
    my $joined = Buf.new;
    whenever $src -> $b {
        $joined.append($b);
        LAST emit $joined;
    }
}.Promise;
is-deeply $joined-result.list, (1, 2, 3), 'on-demand whenever source values reach the body';

my $static-result = await supply {
    my $joined = Buf.new;
    whenever Supply.from-list(Blob.new(4), Blob.new(5)) -> $b {
        $joined.append($b);
        LAST emit $joined;
    }
}.Promise;
is-deeply $static-result.list, (4, 5), 'static whenever source still replays';
