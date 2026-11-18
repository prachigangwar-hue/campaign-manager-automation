from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os

pd.set_option('display.max_columns', None)


#extract last 3 months active LR customers
# Logic : 1. Customer must have LR app installed
# 2. Has session in the LR APP
query = f"""
SELECT toString(mobile) as Customer_Mobile,
toString(1) as seg_lr_app_active
from temp_db.hs_customer_master hcm 
WHERE customer_type <>'vm'
and date(hcm.lr_app_install_date) >= date_sub(today(),90)
and hcm.lr_app_uninstall_date  is null
"""
df = get_ch_client().query_df(query)

df.to_parquet(f'D:/database/seg/seg_lr_app_active/seg_lr_app_active.parquet')