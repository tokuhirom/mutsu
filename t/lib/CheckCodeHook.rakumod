# Test helper for t/modules/compunit/compunit-add-check-code.t: its `sub EXPORT`
# queues closures on the importing unit's `$*CU`, like `static::CLI` does.
sub EXPORT() {
    my $cu = $*CU;
    my @log;
    @log.push("export on {$cu.^name}");
    $cu.add-check-code({ @log.push("first check code") });
    $cu.add-check-code(-> *@args { @log.push("second check code, {@args.elems} args, same cu: {$cu === $cu}") });
    Map.new('@CHECK-LOG' => @log)
}
