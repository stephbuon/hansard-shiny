import re
import sys
import os
import csv
import pandas as pd

#data = {'debate':['Engl likes fishing.', 'Then England went fishing', 'After, England watched fishing.', 'Then England is eating.'],
#        'Age':[20, 21, 19, 18]}

def cooccurance_count(row, nation, concern):

    nation = re.compile(r'\b%s\b' % nation, re.I)
    concern = re.compile(r'\b%s\b' % concern, re.I)

    count = 0
    if (re.search(nation, row)) and (re.search(concern, row)):
        count += 1
    else:
        count = count
    return count


def read_kw_list(kw):
    kw_list = []
    with open(kw, 'r') as csv_file:
        reader = csv.reader(csv_file)
        for row in reader:
            for item in row:
                kw_list.append(item)
    return kw_list


def data_process(df, nations, concerns):
    for nation in nations:
        for concern in concerns:
        
            df['bool'] = df['debate'].apply(cooccurance_count, args = (nation, concern))
            total = df['bool'].sum()

            save_path = '/users/sbuongiorno'
            file_name = 'nation_concern_count.txt'
            
            export_file = os.path.join(save_path, file_name)

            if total != 0:
                with open(export_file, 'a') as f:
                    f.write(nation + ',' + concern + ',' + str(total) + '\n')
                    f.close()


if __name__ == '__main__':
    try:
        input_file = sys.argv[1]
    except IndexError:
        exit('Missing input file argument')

    try:
        input_kw1 = sys.argv[2]
    except IndexError:
        exit('Missing set of keywords. Must provide two')
    
    try:
        input_kw2 = sys.argv[3]
    except IndexError:
        exit('Missing set of keywords. Must provide two')
    
    output_folder = '/users/sbuongiorno' + '/co_occurance_keywords'

    if not os.path.exists(output_folder):
        os.mkdir(output_folder)

    df = pd.read_csv(input_file)

    kw1 = read_kw_list(input_kw1)
    kw2 = read_kw_list(input_kw2)

    data_process(df, kw1, kw2)