# A role whose body `my constant` initializer records each run (#16255).
unit module RoleConstInitOnce;
our @runs;
my role Holder {
    my constant HIDDEN = do { @runs.push('init'); Mu.new };
    method a { HIDDEN.WHICH }
    method b { use nqp; HIDDEN.WHICH }
}
our sub mk(\h) is export { h does Holder }
our sub runs() is export { @runs.elems }
