CREATE OR REPLACE TABLE temp_db.an_clustering_model_customer_list_2326
ENGINE = MergeTree
ORDER BY CUSTOMER_MOBILE AS
Select a.CUSTOMER_MOBILE,
min(a.FIRST_VISITED_ON) as FIRST_VISITED_ON,
max(DATE(a.BILLDATE)) AS LAST_VISITED_ON
from temp_db.an_transaction_data_2126_with_combined_category a
WHERE
a.CUSTOMER_MOBILE NOT IN ('9999999999', '9999999990', '9999999993')
AND length(a.CUSTOMER_MOBILE) = 10 
AND match(a.CUSTOMER_MOBILE, '^([6789][0-9]{9})$')
AND NOT match(a.CUSTOMER_MOBILE, '^(0000000000|1111111111|2222222222|3333333333|4444444444|5555555555|6666666666|7777777777|8888888888|9999999999)$')
AND
a.DIVISION IN
(
'Boys',
'FMCG Food',
'FMCG Non Food',
'Foot Mart',
'Girls',
'Home Mart',
'Infant',
'Lifestyle',
'Lifestyle-Non Apps',
'Men',
'Smart Gadget',
'Staples',
'Toys N Games',
'Women',
'Women Ethnic',
'Women Western'
)
AND DATE(a.BILLDATE) >= '2023-04-01' AND DATE(a.BILLDATE) < '2026-04-01'
GROUP BY 1;