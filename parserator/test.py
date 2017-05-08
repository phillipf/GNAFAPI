import itertools
import operator

L = [('grape', 100), ('grape', 3), ('apple', 15), ('apple', 10),
     ('apple', 4), ('banana', 3)]

def accumulate(l):
    it = itertools.groupby(l, operator.itemgetter(0))
    for key, subiter in it:
       yield key, sum(item[1] for item in subiter)

def accumulate(l):
    it = itertools.groupby(l, operator.itemgetter(1))
    for key, subiter in it:
       yield key, ' '.join(item[0] for item in subiter)