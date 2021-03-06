from collections import defaultdict

import GNAF
import re
import itertools
import operator

import pandas as pd

unlabelled = pd.read_csv("C:/Users/farrelp1/Documents/GNAFAPI/parserator/dataunlabeled_2017-04-21DHHS_CTAddress.csv", header=None)

unlabelled = unlabelled[0].apply(lambda x: re.sub(r', Australia$', "", x))

#unlabelled[unlabelled.apply(lambda x: x.endswith("Australia"))]

def accumulate(l):
    it = itertools.groupby(l, operator.itemgetter(1))
    for key, subiter in it:
       yield key, ' '.join(item[0] for item in subiter)

labelled = unlabelled.apply(lambda x: GNAF.parse(x))

labelled = labelled.apply(lambda x: list(accumulate(x)))

cols = ['RAW_STRING', 'BUILDING_NAME', 'LEVEL_TYPE', 'LEVEL_NUMBER', 'FLAT_TYPE', 'FLAT_NUMBER_PREFIX', 'FLAT_NUMBER',
        'NUMBER_FIRST', 'NUMBER_FIRST_SUFFIX',  'NUMBER_LAST', 'STREET_NAME', 'STREET_TYPE_CODE', 'LOCALITY_NAME',
        'POSTCODE', 'STATE_ABBREVIATION']

df = pd.DataFrame(columns=cols)

df['RAW_STRING'] = unlabelled[0]

for i, row in enumerate(labelled):
    for item in row:
        df[item[0]][i] = item[1]

df.to_csv('labels_2017-05-08DHHS_CTAddress2.csv')
