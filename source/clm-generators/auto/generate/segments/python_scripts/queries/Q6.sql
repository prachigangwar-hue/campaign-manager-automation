--------------------------------------------------------------------
-- Step 2: Join Aggregates and Calculate Super-Category Percentages (Attempt 3 with identity function)
--------------------------------------------------------------------
-- This table joins the base list, the flags table (with percentages), and the super-aggregates table.
-- It contains all features needed for the final segmentation logic.
-- MODIFIED: Explicitly selecting and aliasing all columns from joined tables.
CREATE OR REPLACE TABLE temp_db.an_clustering_model_customer_features_combined_2326
ENGINE = MergeTree
ORDER BY CUSTOMER_MOBILE AS
SELECT
   base.CUSTOMER_MOBILE AS CUSTOMER_MOBILE,
   base.FIRST_VISITED_ON AS FIRST_VISITED_ON,
   base.LAST_VISITED_ON AS LAST_VISITED_ON,
   -- Include all columns from the flags table with explicit aliases
   flags.TWO_YEAR_REVENUE AS TWO_YEAR_REVENUE,
   flags.BILLDAYS AS BILLDAYS,
   flags.BILLS AS BILLS,
   flags.DISTINCT_CATEGORIES AS DISTINCT_CATEGORIES,
   flags.REVENUE_PER_VISIT_DAY AS REVENUE_PER_VISIT_DAY,
   flags.Boys_Junior_PCT_REVENUE AS Boys_Junior_PCT_REVENUE, flags.Boys_Senior_PCT_REVENUE AS Boys_Senior_PCT_REVENUE, flags.Toddler_PCT_REVENUE AS Toddler_PCT_REVENUE, flags.FMCG_Food_PCT_REVENUE AS FMCG_Food_PCT_REVENUE,
   flags.Infant_PCT_REVENUE AS Infant_PCT_REVENUE, flags.FMCG_Non_Food_PCT_REVENUE AS FMCG_Non_Food_PCT_REVENUE, flags.Women_PCT_REVENUE AS Women_PCT_REVENUE, flags.Men_PCT_REVENUE AS Men_PCT_REVENUE,
   flags.Girls_Junior_PCT_REVENUE AS Girls_Junior_PCT_REVENUE, flags.Girls_Senior_PCT_REVENUE AS Girls_Senior_PCT_REVENUE, flags.Others_PCT_REVENUE AS Others_PCT_REVENUE, flags.Home_Mart_PCT_REVENUE AS Home_Mart_PCT_REVENUE,
   flags.Boys_PCT_REVENUE AS Boys_PCT_REVENUE, flags.Girls_PCT_REVENUE AS Girls_PCT_REVENUE, flags.Staples_PCT_REVENUE AS Staples_PCT_REVENUE, flags.Toys_PCT_REVENUE AS Toys_PCT_REVENUE,
   flags.Women_Ethnic_PCT_REVENUE AS Women_Ethnic_PCT_REVENUE, flags.Women_Western_PCT_REVENUE AS Women_Western_PCT_REVENUE,
   flags.Boys_Junior_PCT_QTY AS Boys_Junior_PCT_QTY, flags.Boys_Senior_PCT_QTY AS Boys_Senior_PCT_QTY, flags.Toddler_PCT_QTY AS Toddler_PCT_QTY, flags.FMCG_Food_PCT_QTY AS FMCG_Food_PCT_QTY,
   flags.Infant_PCT_QTY AS Infant_PCT_QTY, flags.FMCG_Non_Food_PCT_QTY AS FMCG_Non_Food_PCT_QTY, flags.Women_PCT_QTY AS Women_PCT_QTY, flags.Men_PCT_QTY AS Men_PCT_QTY,
   flags.Girls_Junior_PCT_QTY AS Girls_Junior_PCT_QTY, flags.Girls_Senior_PCT_QTY AS Girls_Senior_PCT_QTY, flags.Others_PCT_QTY AS Others_PCT_QTY, flags.Home_Mart_PCT_QTY AS Home_Mart_PCT_QTY,
   flags.Boys_PCT_QTY AS Boys_PCT_QTY, flags.Girls_PCT_QTY AS Girls_PCT_QTY, flags.Staples_PCT_QTY AS Staples_PCT_QTY, flags.Toys_PCT_QTY AS Toys_PCT_QTY,
   flags.Women_Ethnic_PCT_QTY AS Women_Ethnic_PCT_QTY, flags.Women_Western_PCT_QTY AS Women_Western_PCT_QTY,
   flags.Boys_Junior_FLAG AS Boys_Junior_FLAG, flags.Boys_Senior_FLAG AS Boys_Senior_FLAG, flags.Toddler_FLAG AS Toddler_FLAG, flags.FMCG_Food_FLAG AS FMCG_Food_FLAG,
   flags.Infant_FLAG AS Infant_FLAG, flags.FMCG_Non_Food_FLAG AS FMCG_Non_Food_FLAG, flags.Women_FLAG AS Women_FLAG, flags.Men_FLAG AS Men_FLAG,
   flags.Girls_Junior_FLAG AS Girls_Junior_FLAG, flags.Girls_Senior_FLAG AS Girls_Senior_FLAG, flags.Others_FLAG AS Others_FLAG, flags.Home_Mart_FLAG AS Home_Mart_FLAG,
   flags.Boys_FLAG AS Boys_FLAG, flags.Girls_FLAG AS Girls_FLAG, flags.Staples_FLAG AS Staples_FLAG, flags.Toys_FLAG AS Toys_FLAG,
   flags.Women_Ethnic_FLAG AS Women_Ethnic_FLAG, flags.Women_Western_FLAG AS Women_Western_FLAG,
   -- Include super-category sums from the aggregates table (agg) with explicit aliases
   agg.Adult_REV_SUM AS Adult_REV_SUM,
   agg.Kids_Toys_REV_SUM AS Kids_Toys_REV_SUM, -- This is correctly aliased here
   agg.Household_Home_FMCG_Staples_REV_SUM AS Household_Home_FMCG_Staples_REV_SUM,
   agg.Others_REV_SUM AS Others_REV_SUM,
   agg.Total_Revenue_2Year AS Total_Revenue_2Year, -- This is correctly aliased here
   -- Calculate Super-category percentages here for convenience, with explicit aliases
   ROUND(agg.Adult_REV_SUM / NULLIF(agg.Total_Revenue_2Year, 0), 4) AS Adult_PCT_REV,
   ROUND(agg.Kids_Toys_REV_SUM / NULLIF(agg.Total_Revenue_2Year, 0), 4) AS Kids_Toys_PCT_REV,
   ROUND(agg.Household_Home_FMCG_Staples_REV_SUM / NULLIF(agg.Total_Revenue_2Year, 0), 4) AS Household_Home_FMCG_Staples_PCT_REV,
   ROUND(agg.Others_REV_SUM / NULLIF(agg.Total_Revenue_2Year, 0), 4) AS Others_PCT_REV
FROM temp_db.an_clustering_model_customer_list_2326 base -- The base list with dates
JOIN temp_db.an_clustering_model_customer_list_2326_binary_flags flags -- Original flags and overall aggregates (including percentages)
 ON base.CUSTOMER_MOBILE = flags.CUSTOMER_MOBILE
JOIN temp_db.an_customer_super_category_aggregates_2326 agg -- Super category aggregates
 ON base.CUSTOMER_MOBILE = agg.CUSTOMER_MOBILE; -- Ensure this join condition is correct
