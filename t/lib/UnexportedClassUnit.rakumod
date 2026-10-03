unit module UnexportedClassUnit;

class Hidden { method value { 'hidden' } }
class Public is export { method value { 'public' } }

sub own-hidden() is export { Hidden.value }
