use Test;

# The set membership and set relation operators are chain-associative, so a
# reduction over one is the conjunction of its pairwise applications (#13794).
# Checked against rakudo.
plan 13;

ok ([(elem)] 1, (1, 2)), '[(elem)]';
ok ([∈] 1, (1, 2)), '[∈]';
nok ([∈] 1, (1, 2), ((1, 2), 3)), '[∈] chains rather than folds';
ok ([(cont)] (1, 2), 1), '[(cont)]';
ok ([∌] (1, 2), 3), '[∌]';
ok ([∉] 5, (1, 2), ((3,),)), '[∉]';
ok ([⊆] set(1), set(1, 2), set(1, 2, 3)), '[⊆]';
nok ([(<=)] set(1), set(1, 2), set(1)), '[(<=)] fails on the last pair';
ok ([⊂] set(1), set(1, 2), set(1, 2, 3)), '[⊂]';
ok ([⊈] set(1, 2), set(1)), '[⊈]';
ok ([⊆]()), 'an empty reduction is True';
is-deeply ([\(elem)] 1, (1, 2), (3,)), (True, True, False).Seq, 'a triangle reduction';
is-deeply ((1, 2) »∈« ((1,), (3,))), ((True,), (False,)), 'a hyper';
