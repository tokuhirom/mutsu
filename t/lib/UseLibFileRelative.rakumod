# Fixture for t/modules/use-lib-file-relative-parse-time.t.
unit module UseLibFileRelative;

constant ALPHA  is export = 1;
constant BETA is export = 2;

our class Widget is export {
    has $.n = 42;
}

our sub make-widget(--> Widget) is export { Widget.new }
