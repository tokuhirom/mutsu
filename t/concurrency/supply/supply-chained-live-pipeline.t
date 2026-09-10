use v6;
use Test;

plan 4;

# A supply block whose whenever wraps ANOTHER on-demand supply rooted at a
# live source must stay open: the chained on-demand source joins the done
# group, so tapping it does not fire a spurious done (the old behavior tore
# down Cro's establish pipelines before any message flowed).
{
    my $in = Supplier::Preserving.new;
    my $t1 = supply { whenever $in.Supply { emit $_ x 2 } }
    my $t2 = supply { whenever $t1 { .emit } }
    my @got;
    my $done = False;
    $t2.tap: { @got.push($_) }, done => { $done = True };
    $in.emit("b");
    is @got.join(","), "bb", 'two-stage on-demand chain delivers the message';
    nok $done, 'no spurious done while the bottom source is live';
}

# A whenever registered dynamically INSIDE another whenever's body (the
# Cro::Connector.establish shape: whenever a connect Promise, then whenever
# the transformer pipeline) also joins the done group, and .Channel on such
# a pipeline taps it live instead of snapshot-draining it.
{
    my $in = Supplier::Preserving.new;
    my $transformed = supply { whenever $in.Supply { emit $_ ~ "!" } }
    my $conn = start { 42 }
    my $established = supply {
        whenever $conn -> $c {
            whenever $transformed -> $msg {
                emit $msg;
            }
        }
    }
    my $ch = $established.Channel;
    $in.emit("jar");
    is $ch.receive, "jar!", 'nested runtime whenever keeps the pipeline live';
    $in.emit("jar2");
    is $ch.receive, "jar2!", 'later messages still flow through the channel';
}
