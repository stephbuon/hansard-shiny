import re
import sys
import os
import csv
import pandas as pd

#data = {'debate':['Engl likes fishing.', 'Then England went fishing', 'After, England watched fishing.', 'Then England is eating.'],
#        'Age':[20, 21, 19, 18]}

# I should have pre-processed this to make sure the data I am going through even has a concern to begin with 


def cooccurance_count(row, nation, concern):
    
    nation = re.compile(r'\b%s\b' % nation, re.I)
    concern = re.compile(r'\b%s\b' % concern, re.I)

    count = 0

    if re.search(nation, row): 
        if re.search(concern, row):
            print('Found coocurance: ' + str(nation) + ' and ' + str(concern))
            count += 1
    else:
        count = count
    return count


def read_kw_list(kw):
    kw_list = []
    with open(kw, 'r') as csv_file:
        reader = csv.reader(csv_file)
        next(reader) 
        for row in reader:
            for item in row:
                kw_list.append(item)
    return kw_list


def data_process(df, nations, concerns):
    
    df['debate'] = df['debate'].str.lower()
    df['debate'] = df['debate'].astype(str)
    
    for nation in nations:
        nation = nation.lower()
        
        for concern in concerns:
            concern = concern.lower()

            #print('Working on ' + nation + ' and ' + concern)
        
            df['bool'] = df['debate'].apply(cooccurance_count, args = (nation, concern))
            df['bool'] = df['bool'].astype(str)

            for index, row in df.iterrows():
                if '1' in row['bool']:
                    print(row['bool'])
                    #row['bool'] = row['bool'] + row['group_count']
                    total = row['group_count']#.sum()                  
                    print(total)

                    decade = df.iloc[0]['decade']

                    save_path = '/users/sbuongiorno'
                    file_name = 'nation_concern_count_' + str(decade) + '.txt'
            
                    export_file = os.path.join(save_path, file_name)

                    #if total != 0:
                    with open(export_file, 'a') as f:
                        f.write(nation + ',' + concern + ',' + str(total) + '\n')
                        f.close()


if __name__ == '__main__':
    try:
        print('Importing main data set.')
        input_file = sys.argv[1]
    except IndexError:
        exit('Missing input file argument')

    try:
        print('Importing keyword set 1/2.')
        input_kw1 = sys.argv[2]
    except IndexError:
        exit('Missing kw1. Must provide two')
    
    try:
        print('Importing keyword set 2/2.')
        input_kw2 = sys.argv[3]
    except IndexError:
        exit('Missing kw2. Must provide two')
    
    output_folder = '/users/sbuongiorno' + '/co_occurance_keywords'

    if not os.path.exists(output_folder):
        os.mkdir(output_folder)

    df = pd.read_csv(input_file)

    kw1 = read_kw_list(input_kw1)
    kw2 = read_kw_list(input_kw2)

    data_process(df, kw1, kw2)

