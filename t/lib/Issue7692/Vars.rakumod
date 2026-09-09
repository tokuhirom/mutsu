unit module Issue7692::Vars;

our $issue7692-value is export = 42;
our $issue7692-only is export = 'only-in-vars';

class I7692Class is export {
    method hi() { 'hi' }
}

sub i7692-exported is export { 'exported' }
