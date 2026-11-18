from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os

pd.set_option('display.max_columns', None)

#FMCG Segment
query = f"""
select distinct CUSTOMER_MOBILE as Customer_Mobile,
ETHNICITY as seg_ethnicity
from temp_db.hs_customer_new_profile_master hlcm 
where exists 
(select 1 from temp_db.hs_vm_customer_master a 
where hlcm.CUSTOMER_MOBILE = a.CUSTOMER_MOBILE)
"""
df = get_ch_client().query_df(query)

df.to_parquet(f'D:/database/seg/seg_ethnicity/seg_ethnicity.parquet')