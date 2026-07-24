unit module SubtestModuleReuse;

role SubtestModuleReuse::Marker is export {
    method marker() { 'marked' }
}

class SubtestModuleReuse::Thing does SubtestModuleReuse::Marker is export {
    method greet() { 'hello' }
}

sub subtest-module-reuse-greeting() is export { 'exported' }

enum SubtestModuleReuseColour is export <SmrRed SmrGreen SmrBlue>;

constant SMR-CONST is export = 42;
