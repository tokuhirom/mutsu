use Test;

# A slang `package-declarator` candidate that parses its own body runs on the
# slang host at parse time (ADR-17741), with a `FOREIGN-LANG` language and its
# actions. `--dump-ast` and `--dump-bytecode` only parse, so none of that code
# may run there (docs/security.md, "Analysis never executes"): the module's
# mainline, the foreign language's actions and the production's action each
# leave a marker file when they run. A real run still runs all three.

plan 8;

my $dir = $*TMPDIR.add("mutsu-dump-ast-no-execute-pkgdecl-$*PID");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir; $dir.rmdir } }

my $mainline = $dir.add('mainline-marker');
my $foreign = $dir.add('foreign-marker');
my $action = $dir.add('action-marker');
$dir.add('PkgDeclNoExec.rakumod').spurt: qq:to/END/;
    use experimental :rakuast;
    '{$mainline.absolute}'.IO.spurt('ran');
    grammar PkgDeclNoExec::Body \{ token TOP \{ \\s* \\w+ \\s* \} \}
    class PkgDeclNoExec::Body-actions \{
        method TOP(\$/) \{
            '{$foreign.absolute}'.IO.spurt('ran');
            my \\type := Metamodel::ClassHOW.new_type(:name(\$*name));
            type.^compose;
            make type;
        \}
    \}
    my role PkgDeclNoExec::G \{
        rule package-declarator:sym<noexec> \{
            <.sym><.kok>
            :my \$*name;
            <longname> \{ \$*name := \$/.hash.AT-KEY('longname').Str \}
            \\\{ <body=.FOREIGN-LANG('NoExec', 'TOP')> \\\}
        \}
    \}
    my role PkgDeclNoExec::A \{
        method package-declarator:sym<noexec>(Mu \$/) \{
            '{$action.absolute}'.IO.spurt('ran');
            my \\type := \$/.hash.AT-KEY('body').made;
            \$*R.current-package.WHO.BIND-KEY(type.^name, type);
            self.attach: \$/, RakuAST::Term::Declaration.new(
                RakuAST::Declaration::ResolvedConstant.new(:compile-time-value(type)));
        \}
    \}
    sub EXPORT(|) \{
        \$*LANG.define_slang('MAIN',
            \$*LANG.slang_grammar('MAIN').^mixin(PkgDeclNoExec::G),
            \$*LANG.slang_actions('MAIN').^mixin(PkgDeclNoExec::A));
        \$*LANG.define_slang('NoExec', PkgDeclNoExec::Body, PkgDeclNoExec::Body-actions);
        BEGIN Map.new
    \}
    END

my $main = $dir.add('main.raku');
$main.spurt: "use lib '{$dir.absolute}';\nuse PkgDeclNoExec;\nnoexec Thing \{ word \};\nsay ::('Thing').^name;\n";

sub mutsu(*@args) {
    my $proc = run $*EXECUTABLE.absolute, |@args, :out, :err;
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out
}

for '--dump-ast', '--dump-bytecode' -> $flag {
    mutsu($flag, $main.absolute);
    nok $mainline.e, "$flag does not run the slang module";
    nok $foreign.e, "$flag does not run the foreign language's actions";
    nok $action.e, "$flag does not run the production's action";
}

is mutsu($main.absolute), "Thing\n", 'a real run declares the package';
ok $mainline.e && $foreign.e && $action.e, 'and runs the module, the foreign actions and the action';

# vim: expandtab shiftwidth=4
