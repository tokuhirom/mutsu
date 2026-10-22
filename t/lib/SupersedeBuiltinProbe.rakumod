# Declares a class in a unit that does NOT import the superseding HOW (#14241).
class SupersedeProbe { }
sub supersede-probe-how is export { SupersedeProbe.HOW.^name }
