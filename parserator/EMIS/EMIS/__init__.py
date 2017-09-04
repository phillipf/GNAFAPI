#!/usr/bin/python
# -*- coding: utf-8 -*-

import pycrfsuite
import os
import re
import string
import warnings
from collections import OrderedDict
from builtins import zip
from builtins import str

import pandas as pd
import probableparsing

#  _____________________
# |1. CONFIGURE LABELS! |
# |_____________________|
#     (\__/) ||
#     (•ㅅ•) ||
#     / 　 づ
LABELS = ['BUILDING_NAME',
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
          'POSTCODE'] # The labels should be a list of strings

#***************** OPTIONAL CONFIG ***************************************************
PARENT_LABEL = 'AddressString'
GROUP_LABEL = 'AddressCollection'

NULL_LABEL = 'Null'

MODEL_FILE    = 'learned_settings.crfsuite'   # filename for the crfsuite settings file

DIRECTIONS = set(['n', 's', 'e', 'w',
                  'ne', 'nw', 'se', 'sw',
                  'north', 'south', 'east', 'west',
                  'northeast', 'northwest', 'southeast', 'southwest'])

#STREET_NAMES = pd.read_csv("data/Street_names.csv")

#STREET_NAMES = STREET_NAMES["Street_names"].to_dict()
#************************************************************************************


try :
    TAGGER = pycrfsuite.Tagger()
    TAGGER.open(os.path.split(os.path.abspath(__file__))[0]+'/'+MODEL_FILE)
except IOError :
    TAGGER = None
    warnings.warn('You must train the model (parserator train [traindata] [modulename]) to create the %s file before you can use the parse and tag methods' %MODEL_FILE)

def parse(address_string):
    tokens = tokenize(address_string)

    if not tokens:
        return []

    features = tokens2features(tokens)

    tags = TAGGER.tag(features)
    return list(zip(tokens, tags))


def tag(address_string, tag_mapping=None):
    tagged_address = OrderedDict()

    last_label = None
    is_intersection = False
    og_labels = []

    for token, label in parse(address_string):
        # if label == 'IntersectionSeparator':
        #     is_intersection = True
        if 'STREET_NAME' in label: #and is_intersection:
            label = 'Second' + label

        # saving old label
        og_labels.append(label)
        # map tag to a new tag if tag mapping is provided
        if tag_mapping and tag_mapping.get(label):
            label = tag_mapping.get(label)
        else:
            label = label

        if label == last_label:
            tagged_address[label].append(token)
        elif label not in tagged_address:
            tagged_address[label] = [token]
        else:
            raise RepeatedLabelError(address_string, parse(address_string),
                                     label)

        last_label = label

    for token in tagged_address:
        component = ' '.join(tagged_address[token])
        component = component.strip(" ,;")
        tagged_address[token] = component

    if 'NUMBER_FIRST' in og_labels: #and not is_intersection:
        address_type = 'Street Address'
        #elif is_intersection and 'AddressNumber' not in og_labels:
        #address_type = 'Intersection'
    # elif 'USPSBoxID' in og_labels:
    #     address_type = 'PO Box'
    else:
        address_type = 'Ambiguous'

    return tagged_address, address_type


def tokenize(address_string):
    if isinstance(address_string, bytes):
        address_string = str(address_string, encoding='utf-8')
    address_string = re.sub('(&#38;)|(&amp;)', '&', address_string)
    re_tokens = re.compile(r"""
    \(*\b[^\s,;#&()-/]+[.,;)\n]*   # ['ab. cd,ef '] -> ['ab.', 'cd,', 'ef']
    |
    [#&]                       # [^'#abc'] -> ['#']
    """,
                           re.VERBOSE | re.UNICODE)

    tokens = re_tokens.findall(address_string)

    if not tokens:
        return []

    return tokens


def tokenFeatures(token):
    if token in (u'&', u'#', u'½'):
        token_clean = token
    else:
        token_clean = re.sub(r'(^[\W]*)|([^.\w]*$)', u'', token,
                             flags=re.UNICODE)

    token_abbrev = re.sub(r'[.]', u'', token_clean.lower())
    features = {
        'abbrev': token_clean[-1] == u'.',
        'digits': digits(token_clean),
        'word': (token_abbrev
                 if not token_abbrev.isdigit()
                 else False),
        'trailing.zeros': (trailingZeros(token_abbrev)
                           if token_abbrev.isdigit()
                           else False),
        'length': (u'd:' + str(len(token_abbrev))
                   if token_abbrev.isdigit()
                   else u'w:' + str(len(token_abbrev))),
        'endsinpunc': (token[-1]
                       if bool(re.match('.+[^.\w]', token, flags=re.UNICODE))
                       else False),
        'directional': token_abbrev in DIRECTIONS,
        #'street_name': token_abbrev in STREET_NAMES,
        'has.vowels': bool(set(token_abbrev[1:]) & set('aeiou')),
    }

    return features


def tokens2features(address):
    feature_sequence = [tokenFeatures(address[0])]
    previous_features = feature_sequence[-1].copy()

    for token in address[1:]:
        token_features = tokenFeatures(token)
        current_features = token_features.copy()

        feature_sequence[-1]['next'] = current_features
        token_features['previous'] = previous_features

        feature_sequence.append(token_features)

        previous_features = current_features

    feature_sequence[0]['address.start'] = True
    feature_sequence[-1]['address.end'] = True

    if len(feature_sequence) > 1:
        feature_sequence[1]['previous']['address.start'] = True
        feature_sequence[-2]['next']['address.end'] = True

    return feature_sequence


def digits(token):
    if token.isdigit():
        return 'all_digits'
    elif set(token) & set(string.digits):
        return 'some_digits'
    else:
        return 'no_digits'


def trailingZeros(token):
    results = re.findall(r'(0+)$', token)
    if results:
        return results[0]
    else:
        return ''

class RepeatedLabelError(probableparsing.RepeatedLabelError):
    REPO_URL = 'https://github.com/datamade/usaddress/issues/new'
    DOCS_URL = 'https://usaddress.readthedocs.io/'