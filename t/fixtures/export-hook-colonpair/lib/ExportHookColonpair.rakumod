module ExportHookColonpair {
    our sub render(\obj, Bool :$pretty = True) {
        sub emit(\o --> Nil) { @*OUT.push($pretty ?? "pretty" !! "compact") }
        my @*OUT;
        emit(obj);
        @*OUT.join
    }
}

# The named argument `:pretty(True)` below is no export key of the hook.
sub EXPORT() {
    my sub helper(\obj) { ExportHookColonpair::render(obj, :pretty(True)) }
    Map.new
}
