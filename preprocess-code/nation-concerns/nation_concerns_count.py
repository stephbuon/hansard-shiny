import re
import sys
import os
import csv
import pandas as pd

def cooccurance_count(row, nation, concerns, decade, debate_id):
    
    count = 0 
    
    nation = nation.lower()
    nation_regex = re.compile(r'\b%s\b' % nation, re.I)
        
    if re.search(nation_regex, row): 
            
        for concern in concerns:
            concern = concern.lower()
            concern_regex = re.compile(r'\b%s\b' % concern, re.I)

            if re.search(concern_regex, row):
                if nation != concern:
                    print('Found coocurance: ' + str(nation) + ' and ' + str(concern))
                
                    save_path = '/users/sbuongiorno'
                    file_name = 'nation_concern_count_' + str(decade) + '.csv'
            
                    export_file = os.path.join(save_path, file_name)

                    with open(export_file, 'a') as f:
                        f.write(nation + ',' + concern + ',' + str(1) + ',' + str(debate_id) + '\n')
                        f.close()
                
    

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
    
    decade = df.iloc[0]['decade']
    
    df['debate'] = df['debate'].str.lower()
    df['debate'] = df['debate'].astype(str)

    for index, row in df.iterrows():
        debate_id = row['debate_id']

        for nation in nations:
            #row['debate'].apply(cooccurance_count, args = (nation, concerns, decade, debate_id))
            cooccurance_count(row['debate'], nation, concerns, decade, debate_id)


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
