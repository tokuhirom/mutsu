unit module ExportedTypeIndex;

role Node is export { }
class Cond does Node is export { has $.x is rw = 1; }
class Loop does Node is export { }
enum ExpColour is export <Red Green Blue>;
constant EXP_MARK is export = 'mark';

class Plain { }
