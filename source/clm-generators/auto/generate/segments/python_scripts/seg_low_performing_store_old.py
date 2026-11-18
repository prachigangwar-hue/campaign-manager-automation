from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os
import shutil

pd.set_option('display.max_columns', None)

dir_path = 'D:/database/seg/seg_low_performing_store'
if os.path.exists(dir_path):
    shutil.rmtree(dir_path)


#language Segment
query = f"""
select Customer_Mobile,store as seg_low_performing_store
from
(select CUSTOMER_MOBILE as Customer_Mobile,
argMax(SHRTNAME,BILLDATE) as Store
from vmart_sales.dt_pos_transactional_data dptd 
where CUSTOMER_MOBILE NOT IN ('9999999999', '9999999990', '9999999993','6000000000') 
    AND length(CUSTOMER_MOBILE) = 10  
    AND match(CUSTOMER_MOBILE, '^[6789][0-9]{{9}}$')
group by 1) a
join
temp_db.low_performing_store b
on lower(a.Store) = lower(b.store)
"""
df = get_ch_client().query_df(query)

os.mkdir(dir_path)

df.to_parquet(os.path.join(dir_path,'seg_low_performing_store.parquet'))