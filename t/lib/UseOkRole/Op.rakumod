unit module UseOkRole::Op;
use UseOkRole::Cap;
role Base { method d { self ~~ Chained } }
role Node does Chained { }
class C does Node is export { }
