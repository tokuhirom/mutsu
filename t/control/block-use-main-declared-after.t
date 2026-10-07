use lib 't/lib';
use Test;

plan 3;

# Reduced from App::Stouch's t/01-stouch.rakutest: a script declares its own
# `BEGIN sub MAIN(|)` and loads a MAIN-exporting module inside a block. The
# module's MAIN is lexical to that block, so it is neither a redeclaration of
# the script's MAIN nor something auto-dispatched at program end (which used to
# print a usage message and exit 2).
BEGIN sub MAIN(|) { };

{
    use PkgMainExport;

    is PkgMainExport::MAIN(3), 'int 3', 'qualified call reaches the module MAIN (Int)';
    is PkgMainExport::MAIN('a', :d('t')), 'str a t', 'qualified call with a named arg';
    is PkgMainExport::MAIN('a'), 'str a dflt', 'qualified call using the default';
}
