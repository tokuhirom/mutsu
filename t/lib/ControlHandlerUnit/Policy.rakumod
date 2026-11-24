unit module ControlHandlerUnit::Policy;
# File-private and package-scoped helpers the CONTROL handlers below call.
sub private-key($x) { "private($x)" }
our sub package-key($x) { "package($x)" }

sub with-private-handler(&cb) is export {
    my @seen;
    {
        cb();
        CONTROL { when CX::Warn { @seen.push: private-key(.message); .resume } }
    }
    @seen
}

sub with-package-handler(&cb) is export {
    my @seen;
    cb();
    CONTROL { when CX::Warn { @seen.push: package-key(.message); .resume } }
    @seen
}
