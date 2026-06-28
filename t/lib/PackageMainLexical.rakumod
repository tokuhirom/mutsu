package PackageMainLexical {
    # A `my` lexical assigned in the package-block mainline, read by an exported
    # MAIN candidate. Before the fix, MAIN ran via the call_function_def slow path
    # whose body compiles under the plain package name (auto-qualifying `$config`
    # to `PackageMainLexical::config`), which the package-lexical resolver did not
    # recognise — so MAIN saw the lexical as undefined.
    my $config = "from-mainline";

    proto MAIN(|) is export {*}
    multi sub MAIN('show') is export { say "config=$config" }
    multi sub MAIN('twice') is export { say "config=$config$config" }
}
