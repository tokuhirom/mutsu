# OOP - class instantiation, method calls, inheritance
class Animal {
    has $.name;
    has $.legs;

    method describe() {
        return $.name ~ " has " ~ $.legs ~ " legs";
    }
}

class Dog is Animal {
    has $.breed;

    method bark() {
        return "Woof!";
    }

    method full-info() {
        return self.describe() ~ " (" ~ $.breed ~ ")";
    }
}

my $checksum = 0;

# Object creation and method calls
for 1..5000 -> $i {
    my $dog = Dog.new(name => "Rex", legs => 4, breed => "Labrador");
    my $desc = $dog.full-info();
    $checksum += $desc.chars;
    $checksum += $dog.bark().chars;
}

# Polymorphism
my @animals;
for 1..2000 -> $i {
    @animals.push(Animal.new(name => "Cat", legs => 4));
    @animals.push(Dog.new(name => "Rex", legs => 4, breed => "Lab"));
}
for @animals -> $a {
    $checksum += $a.describe().chars;
}

say "checksum = $checksum";
