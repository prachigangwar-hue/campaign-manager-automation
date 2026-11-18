from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os

pd.set_option('display.max_columns', None)

#FMCG Segment
query = f"""SELECT
    CUSTOMER_MOBILE AS Customer_Mobile,
    CASE 
        WHEN arraySort(groupUniqArray(DIVISION)) = arraySort(['Staples', 'FMCG Food', 'FMCG Non Food']) THEN 'All_Dept_Loyalist'
        WHEN arraySort(groupUniqArray(SECTION)) = arraySort(['Cooking Medium', 'Pulses', 'Basic Staples', 'Spices', 'Flours']) THEN 'Staple_Champion'
        WHEN has(groupUniqArray(SECTION), 'Cooking Medium')
          OR has(groupUniqArray(SECTION), 'Pulses')
          OR has(groupUniqArray(SECTION), 'Basic Staples')
          OR has(groupUniqArray(SECTION), 'Spices')
          OR has(groupUniqArray(SECTION), 'Flours') THEN 'Intermittent_Essential'
        WHEN has(groupUniqArray(DIVISION), 'FMCG Food')
        	OR has(groupUniqArray(DIVISION), 'FMCG Non Food')
        	AND NOT hasAny(groupUniqArray(SECTION), ['Cooking Medium', 'Pulses', 'Basic Staples', 'Spices', 'Flours']) THEN 'Category Selectives'
        WHEN has(groupUniqArray(DIVISION), 'Staples')
          OR has(groupUniqArray(DIVISION), 'FMCG Food')
          OR has(groupUniqArray(DIVISION), 'FMCG Non Food') THEN 'General_Fmcg_Buyer'
        ELSE 'Fmcg_Potential_Buyer'
    END AS seg_fmcg_new
FROM vmart_sales.dt_pos_transactional_data
WHERE CUSTOMER_MOBILE GLOBAL IN (
    SELECT CUSTOMER_MOBILE
    FROM vmart_sales.dt_pos_transactional_data
    GROUP BY CUSTOMER_MOBILE
    HAVING COUNT(DISTINCT DATE(BILLDATE)) > 2
       AND COUNT(DISTINCT DATE(BILLDATE)) < 14
)
GROUP BY CUSTOMER_MOBILE
UNION ALL
SELECT
    CUSTOMER_MOBILE,
    CASE 
        WHEN arraySort(groupUniqArray(DIVISION)) = arraySort(['Staples', 'FMCG Food', 'FMCG Non Food']) THEN 'All_Dept_Occasional_Shopper'
         WHEN has(groupUniqArray(SECTION), 'Cooking Medium')
          OR has(groupUniqArray(SECTION), 'Pulses')
          OR has(groupUniqArray(SECTION), 'Basic Staples')
          OR has(groupUniqArray(SECTION), 'Spices')
          OR has(groupUniqArray(SECTION), 'Flours') THEN 'Intermittent_Essential'
        ELSE 'Fmcg_Potential_Buyer'
    END AS seg_fmcg_new
FROM vmart_sales.dt_pos_transactional_data
WHERE CUSTOMER_MOBILE GLOBAL IN (
    SELECT CUSTOMER_MOBILE
    FROM vmart_sales.dt_pos_transactional_data
    GROUP BY CUSTOMER_MOBILE
    HAVING COUNT(DISTINCT DATE(BILLDATE)) > 1
       AND COUNT(DISTINCT DATE(BILLDATE)) < 3
)
GROUP BY 1"""
df = get_ch_client().query_df(query)

df.to_parquet(f'D:/database/seg/seg_fmcg_new/seg_fmcg_new.parquet')