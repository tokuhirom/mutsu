use Test;

plan 14;

# push on hash element
{
    my %h;
    %h<key> = [1,2,3];
    %h<key>.push(4);
    is +%h<key>, 4, 'push on hash element increases count';
    is %h<key>[3], 4, 'push on hash element adds correct value';
}

# push on array element
{
    my @a = [1,2], [3,4];
    @a[0].push(5);
    is +@a[0], 3, 'push on array element increases count';
    is @a[0][2], 5, 'push on array element adds correct value';
}

# pop on hash element
{
    my %h;
    %h<key> = [1,2,3];
    is %h<key>.pop, 3, 'pop on hash element returns removed value';
    is +%h<key>, 2, 'pop on hash element decreases count';
}

# unshift on hash element
{
    my %h;
    %h<key> = [1,2,3];
    %h<key>.unshift(0);
    is +%h<key>, 4, 'unshift on hash element increases count';
    is %h<key>[0], 0, 'unshift on hash element adds to front';
}

# append on hash element
{
    my %h;
    %h<key> = [1,2,3];
    %h<key>.append(4,5);
    is +%h<key>, 5, 'append on hash element adds multiple values';
    is %h<key>[3], 4, 'append on hash element first appended value';
    is %h<key>[4], 5, 'append on hash element second appended value';
}

# shift on hash element
{
    my %h;
    %h<key> = [1,2,3];
    is %h<key>.shift, 1, 'shift on hash element returns removed value';
    is +%h<key>, 2, 'shift on hash element decreases count';
    is %h<key>[0], 2, 'shift on hash element removes from front';
}
