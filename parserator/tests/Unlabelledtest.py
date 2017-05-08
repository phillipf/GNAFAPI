from collections import defaultdict

import GNAF
import itertools
import operator

import pandas as pd

unlabelled = pd.read_csv("C:/Users/farrelp1/Documents/GNAFAPI/parserator/dataunlabeled_2017-04-21DHHS_CTAddress.csv", header=None)

def accumulate(l):
    it = itertools.groupby(l, operator.itemgetter(1))
    for key, subiter in it:
       yield key, ' '.join(item[0] for item in subiter)

labelled = unlabelled[0].apply(lambda x: GNAF.parse(x))

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

labelled.pivot(columns=0, values=1,)

#grouped_things = defaultdict(list)

things = labelled

grouped_things = defaultdict(list)
for thing in things:
    for item in thing:
        grouped_things[item[0]].append(item[1])
        #if len(grouped_things[item[1]]) > 1:
            #grouped_things[item[1]] = " ".join(grouped_things[item[1]])

        #grouped_things[item[1]]." ".join(item[0])

df = pd.DataFrame.from_dict(grouped_things, orient='index').T

df['RAW_STRING'] = unlabelled[0]

cols = df.columns.tolist()


df = df[cols]



#for key in grouped_things:
    #print key
    #for thing in grouped_things[key]:
        #if len(thing) > 1:
            #grouped_things[key] = " ".join(thing)
    #print


#grouped_things2 = defaultdict(list)
#labelled2 = list()

#for row in labelled:
    #for thing in row:
       #grouped_things[thing[1]] + thing[0]
       #grouped_things[thing[1]].append(thing[0])
        #for key in grouped_things:
            #grouped_things2[key] = [" ".join(grouped_things[key])]







df = pd.DataFrame(labelled)

df = pd.DataFrame(labelled, columns=['BUILDING_NAME',
          'LOT_NUMBER_PREFIX',
          'LOT_NUMBER',
          'LOT_NUMBER_SUFFIX',
          'FLAT_TYPE',
          'FLAT_NUMBER_PREFIX',
          'FLAT_NUMBER',
          'FLAT_NUMBER_SUFFIX',
          'LEVEL_TYPE',
          'LEVEL_NUMBER_PREFIX',
          'LEVEL_NUMBER',
          'LEVEL_NUMBER_SUFFIX',
          'NUMBER_FIRST_PREFIX',
          'NUMBER_FIRST',
          'NUMBER_FIRST_SUFFIX',
          'NUMBER_LAST_PREFIX',
          'NUMBER_LAST',
          'NUMBER_LAST_SUFFIX',
          'STREET_NAME',
          'STREET_TYPE_CODE',
          'LOCALITY_NAME',
          'STATE_ABBREVIATION',
          'POSTCODE'])

state = ()
for i in labelled:
    state += (i,)

labelled[0]

df.pivot(columns=0, values=1)

#GNAF.parse(x)