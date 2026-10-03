unit module OwnTypeOverCallerEnum;

class Error { method kind() { 'own class' } }

sub own-error-kind() is export { Error.kind }
