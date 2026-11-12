use Test;

class Base {
    method label() { 'base' }
}
class Later is Base {
    my $.label = 'child';
}
is Later.label, 'child', 'child class accessor wins on type object';
is Later.new.label, 'child', 'child class accessor wins on instance';
dies-ok { Later.label(1) }, 'class accessor rejects extra arguments';
dies-ok { Later.new.label(1) }, 'instance accessor rejects extra arguments';

class Middle is Base {
    my $.label = 'middle';
}
class Grandchild is Middle { }
is Grandchild.label, 'middle', 'inherited class accessor wins on type object';
is Grandchild.new.label, 'middle', 'inherited class accessor wins on instance';

class Override is Middle {
    method label() { 'override' }
}
is Override.label, 'override', 'child method wins over inherited class accessor';
is Override.new.label, 'override', 'child instance method wins';

done-testing;
