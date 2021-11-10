# after running collocates that put year and sentence ID in same column: 

import pandas as pd

collocates_w_id = pd.read_csv('/users/sbuongiorno/all_collocates_w_id.csv')
collocates_w_id[['year', 'sentence_id']] = collocates_w_id['year'].str.split('-', expand = True)

hansard = pd.read_csv('/scratch/group/pract-txt-mine/sbuongiorno/hansard_c19_improved_speaker_names.csv')
hansard = hansard[['sentence_id','new_speaker']]

collocates_w_id = collocates_w_id.merge(hansard, how='left')

collocates_w_id.to_csv('/scratch/group/pract-txt-mine/sbuongiorno/speaker_collocates.csv')