import re
import GNAF
import itertools
import operator

import pandas as pd

unlabelled = pd.read_csv(
    "N:/ABR/Output 1.1 New Trade Waste Customers for 2016-17/8. Gentrack/190617_unlabelled_Gentrack.csv", header=None)

site = unlabelled[0]

unlabelled = unlabelled[1]


def accumulate(l):
    it = itertools.groupby(l, operator.itemgetter(1))
    for key, subiter in it:
        yield key, ' '.join(item[0] for item in subiter)


labelled = unlabelled.apply(lambda x: GNAF.parse(x))

labelled = labelled.apply(lambda x: list(accumulate(x)))

cols = ['SITE', 'ADDRESS_STRING', 'BUILDING_NAME', 'LOT_NUMBER_PREFIX', 'LOT_NUMBER', 'LOT_NUMBER_SUFFIX', 'LEVEL_TYPE',
        'LEVEL_NUMBER', 'FLAT_TYPE', 'FLAT_NUMBER_PREFIX', 'FLAT_NUMBER', 'FLAT_NUMBER_SUFFIX',
        'NUMBER_FIRST', 'NUMBER_FIRST_SUFFIX', 'NUMBER_LAST', 'STREET_NAME', 'STREET_TYPE_CODE', 'LOCALITY_NAME',
        'POSTCODE', 'STATE_ABBREVIATION']

df = pd.DataFrame(columns=cols)

df['SITE'] = site

df['ADDRESS_STRING'] = unlabelled

for i, row in enumerate(labelled):
    for item in row:
        df[item[0]][i] = item[1]

df2 = df.ix[1:]

df2.to_csv('N:/ABR/Output 1.1 New Trade Waste Customers for 2016-17/8.Gentrack/190617_labelled_Gentrack.csv',
           index=False)