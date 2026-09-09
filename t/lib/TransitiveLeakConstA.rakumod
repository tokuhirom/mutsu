unit module TransitiveLeakConstA;
constant SHARED-CONST-NAME = 'from-A';
sub a-reads-shared() is export { SHARED-CONST-NAME }
