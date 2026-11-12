unit module ReqScope::Loader;
use ReqScope::Roles;
sub load-and-parse($module, $text) is export {
    require ::($module);
    ::($module).WHO<&parse-it>($text)
}
sub load-and-origin($module) is export {
    require ::($module);
    ::($module).WHO<&origin>()
}
