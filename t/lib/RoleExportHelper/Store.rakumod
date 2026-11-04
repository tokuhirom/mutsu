unit class RoleExportHelper::Store;

role Writer is export {
    sub render($what) returns Str { $what.raku }
    sub to-text($what) is export { render($what) }
    method to-text() { &to-text(self) }
}
