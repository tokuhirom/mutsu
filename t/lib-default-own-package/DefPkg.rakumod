module DefPkg {
    sub fallback() { 2 }
    enum Mode is export (Fast => 1, Slow => 2);
    sub describe(Str $name, Mode $m = Mode(fallback())) is export { "$name " ~ $m.key }
}
