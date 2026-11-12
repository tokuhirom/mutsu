# An exported `prefix:<!>` over a class of its own (the shape of Red's operators).
unit module JunctionNotOp;
class Node is export { }
multi prefix:<!>(Node $a) is export { 'node' }
