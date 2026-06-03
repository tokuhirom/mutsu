use Test;

plan 6;

# A reduction operator used as the inner op of a hyper op applies the base op
# to each pair (reducing a binary op over two operands is the op itself).
is ((1, 2) >>[+]<< (100, 200)).join(','), '101,202', '>>[+]<< adds pairwise';
is ((1, 2, 3) >>[*]<< (2, 3, 4)).join(','), '2,6,12', '>>[*]<< multiplies pairwise';
is (('a', 'b') >>[~]<< ('x', 'y')).join(','), 'ax,by', '>>[~]<< concats pairwise';

# dwim forms
is ((1, 2, 3) >>[+]>> 10).join(','), '11,12,13', '>>[+]>> with scalar';
is (10 <<[+]<< (1, 2, 3)).join(','), '11,12,13', '<<[+]<< with scalar';

# longer list, same op
is ((1, 2, 3, 4) >>[-]<< (1, 1, 1, 1)).join(','), '0,1,2,3', '>>[-]<< subtracts pairwise';
