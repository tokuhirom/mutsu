use lc-export-hook;
unit module LcExportUser;
sub direct is export { lc-export-hook(1) }
sub viaeval is export { EVAL 'lc-export-hook(2)' }
sub viaast is export {
    RakuAST::Call::Name.new(
        name => RakuAST::Name.from-identifier("lc-export-hook"),
        args => RakuAST::ArgList.new(RakuAST::StrLiteral.new("3")),
    ).EVAL
}
