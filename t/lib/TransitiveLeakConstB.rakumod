unit module TransitiveLeakConstB;
constant SHARED-CONST-NAME = 'from-B';
sub b-reads-shared() is export { SHARED-CONST-NAME }
