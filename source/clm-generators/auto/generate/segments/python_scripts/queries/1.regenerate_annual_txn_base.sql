-- CREATE THE CURRENT YEAR ANNUAL TABLE
-- NEED TO MAKE CHANGES WHEN NEW FY BEGINS
CREATE OR REPLACE TABLE temp_db.an_segmentation_base_2526
ENGINE = MergeTree
ORDER BY CUSTOMER_MOBILE AS
select * from
(Select a.*, b.FIRST_VISITED_ON as FIRST_VISITED_ON
from vmart_sales.dt_pos_transactional_data a
global LEFT JOIN customers.rfm_till_date b
on a.CUSTOMER_MOBILE = b.CUSTOMER_MOBILE
WHERE
a.CUSTOMER_MOBILE NOT IN ('9999999999', '9999999990', '9999999993')
AND length(a.CUSTOMER_MOBILE) = 10 
AND match(a.CUSTOMER_MOBILE, '^([6789][0-9]{9})$')
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
AND DATE(a.BILLDATE) >= '2025-04-01' AND DATE(a.BILLDATE) < '2026-04-01')
