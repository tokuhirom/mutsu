unit module Issue9925::Vars;

our $i9925-value is export = 'imported';
our $i9925-direction is export = -1;

class I9925Class is export {
    method hi() { 'hi' }
}

sub i9925-tag($x) is export { "<$x>" }
