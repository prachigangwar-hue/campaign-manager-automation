from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os
import shutil

pd.set_option('display.max_columns', None)

dir_path = 'D:/database/seg/seg_active_store'
if os.path.exists(dir_path):
    shutil.rmtree(dir_path)


#language Segment
query = f"""
select CUSTOMER_MOBILE as Customer_Mobile,ACTIVE as seg_active_store
from
(select CUSTOMER_MOBILE,argMax(SHRTNAME,BILLDATE) as store
from vmart_sales.dt_pos_transactional_data
where DATE(BILLDATE)>='2022-01-01'
and CUSTOMER_MOBILE NOT IN ('9999999999', '9999999990', '9999999993','6000000000') 
AND length(CUSTOMER_MOBILE) = 10  
AND match(CUSTOMER_MOBILE, '^[6789][0-9]{{9}}$')
group by 1) a
join
vmart_sales.stores s 
on a.store = s.STORE_NAME
"""
df = get_ch_client().query_df(query)

os.mkdir(dir_path)

df.to_parquet(os.path.join(dir_path,'seg_active_store.parquet'))