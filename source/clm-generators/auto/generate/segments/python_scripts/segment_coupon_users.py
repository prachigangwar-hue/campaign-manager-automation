from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os

pd.set_option('display.max_columns', None)

#FMCG Segment
query = f"""
SELECT toString(CUSTOMER_MOBILE) AS Customer_Mobile,
CASE
	WHEN COUPON_USES >3 THEN '3+'
	ELSE toString(COUPON_USES)
END AS seg_coupon_users
FROM
(SELECT CUSTOMER_MOBILE,COUNT(DISTINCT COUPON_CODE) AS COUPON_USES
FROM
(select DISTINCT CUSTOMER_MOBILE,COUPON_CODE 
FROM vmart_sales.dt_pos_transactional_data
WHERE COUPON_CODE  <> ''
AND CUSTOMER_MOBILE NOT IN ('9999999999', '9999999990', '9999999993') 
  AND length(CUSTOMER_MOBILE) = 10  
  AND match(CUSTOMER_MOBILE, '^([6789][0-9]{{9}})$')
  AND NOT match(CUSTOMER_MOBILE, '^(0000000000|1111111111|2222222222|3333333333|4444444444|5555555555|6666666666|7777777777|8888888888|9999999999)$')
  )
GROUP BY 1
ORDER BY 2 DESC)
"""
df = get_ch_client().query_df(query)

try:
    if not(df.empty):
        df.to_parquet(f'D:/database/seg/seg_coupon_users/seg_coupon_users.parquet')
        print("file saved successfully!!")
    else:
        print("failed to save as empty data table!!")
except:
    print("something went wrong!!")

