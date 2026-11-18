from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os

pd.set_option('display.max_columns', None)

#FMCG Segment
query = f"""SELECT DISTINCT CUSTOMER_MOBILE as Customer_Mobile, '1' AS seg_teenz 
FROM (
    SELECT CUSTOMER_MOBILE, SUM(QTY) AS TOTAL_QTY, SUM(NETAMT) AS TOTAL_SALE
    FROM vmart_sales.dt_pos_transactional_data
    WHERE DIVISION IN ('Boys','Girls')
      AND BILLDATE >= '2023-01-01' AND BILLDATE < '2024-01-01'
      AND SIZE REGEXP '/(11-12|12-13|13-14)'
    GROUP BY CUSTOMER_MOBILE
    HAVING TOTAL_QTY > 0 AND TOTAL_SALE > 0

    UNION ALL

    SELECT CUSTOMER_MOBILE, SUM(QTY) AS TOTAL_QTY, SUM(NETAMT) AS TOTAL_SALE
    FROM vmart_sales.dt_pos_transactional_data
    WHERE DIVISION IN ('Boys','Girls')
      AND BILLDATE >= '2024-01-01' AND BILLDATE < '2025-01-01'
      AND SIZE REGEXP '/(12-13|13-14|14-15)'
    GROUP BY CUSTOMER_MOBILE
    HAVING TOTAL_QTY > 0 AND TOTAL_SALE > 0

    UNION ALL

    SELECT CUSTOMER_MOBILE, SUM(QTY) AS TOTAL_QTY, SUM(NETAMT) AS TOTAL_SALE
    FROM vmart_sales.dt_pos_transactional_data
    WHERE DIVISION IN ('Boys','Girls')
      AND BILLDATE >= '2025-01-01' AND BILLDATE < today()
      AND SIZE REGEXP '/(13-14|14-15|15-16)'
    GROUP BY CUSTOMER_MOBILE
    HAVING TOTAL_QTY > 0 AND TOTAL_SALE > 0
)
WHERE CUSTOMER_MOBILE NOT IN ('9999999999', '9999999990', '9999999993') 
  AND length(CUSTOMER_MOBILE) = 10  
  AND match(CUSTOMER_MOBILE, '^([6789][0-9]{{9}})$')
"""
df = get_ch_client().query_df(query)

df.to_parquet(f'D:/database/seg/seg_teenz/seg_teenz.parquet')