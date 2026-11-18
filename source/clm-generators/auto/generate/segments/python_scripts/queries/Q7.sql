-- Step 4: Assign Value Tier AND INTUITIVE Inferred Household Structure (Final Output)
--------------------------------------------------------------------
-- This is the FINAL table containing all features and the two segmentation columns
CREATE OR REPLACE TABLE temp_db.an_customer_personas_intuitive
ENGINE = MergeTree
ORDER BY CUSTOMER_MOBILE AS
SELECT
   *, -- Select ALL columns from the combined features table
   -- Assign Value Tier based on Percentile Rank of 2-Year Rolling Revenue
   CASE
       WHEN ntile(4) OVER (ORDER BY TWO_YEAR_REVENUE ASC) = 4 THEN 'High_Value'
       WHEN ntile(4) OVER (ORDER BY TWO_YEAR_REVENUE ASC) = 3 THEN 'Mid_Value'
       WHEN ntile(4) OVER (ORDER BY TWO_YEAR_REVENUE ASC) = 2 THEN 'Low_Value'
       WHEN ntile(4) OVER (ORDER BY TWO_YEAR_REVENUE ASC) = 1 THEN 'Very_Low_Value'
       ELSE 'Unassigned Value Tier' -- Should not happen
   END AS Customer_Value_Tier,
   --------------------------------------------------------------------
   -- Assign Intuitive Inferred Household Structure
   --------------------------------------------------------------------
   CASE
       -- Define Adult Presence Flag using flags from the combined features table
       WHEN (Men_FLAG = 1 OR Women_FLAG = 1 OR Women_Ethnic_FLAG = 1 OR Women_Western_FLAG = 1) THEN
           -- Customer buys Adult Apparel/Footwear etc.
           -- Check if they also buy for Kids/Toys using the super category sum
           CASE
               WHEN Kids_Toys_REV_SUM > 0 THEN
                   -- They buy Adult items AND Kids/Toys items --> Family with Adults
                   -- Now, infer kid age/gender mix based on specific flags
                   CASE
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) AND (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) AND (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Adults_With_Young_and_Older_Mixed_Kids'
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) AND (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) THEN 'Family_Adults_With_Young_and_Older Boys'
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) AND (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Adults_With_Young_and_Older_Girls'
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) THEN 'Family_Adults_With_Only_Young_Children' -- Implies no specific Older flags set
                       WHEN (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) AND (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Adults_with_Only_Older_Mixed_Gender_Kids' -- Implies no Young specific flags set
                       WHEN (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) THEN 'Family_Adults_With_Only_Older_Boys' -- Implies no Young specific, no Older Girls flags
                       WHEN (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Adults_With_Only_Older_Girls' -- Implies no Young specific, no Older Boys flags
                       -- Fallback for general Kids/Toys purchases when Adults are present, but no *specific* age/gender flags triggered
                       ELSE 'Family_Adults_With_Kids_or_Toys_Age_or_Gender_Unknown' -- Covers Toys, general Boy/Girl category without specific age flags
                   END
               ELSE
                   -- They buy Adult items, but NOT Kids/Toys items --> Adults Only (No Kids)
                   -- Now, infer adult gender mix based on Men/Women flags
                   CASE
                       WHEN Men_FLAG = 1 AND (Women_FLAG = 1 OR Women_Ethnic_FLAG = 1 OR Women_Western_FLAG = 1) THEN 'Adults_Only_Couple'
                       WHEN Men_FLAG = 1 THEN 'Adults_Only_Single_Male'
                       WHEN (Women_FLAG = 1 OR Women_Ethnic_FLAG = 1 OR Women_Western_FLAG = 1) THEN 'Adults_Only_Single_Female'
                       ELSE 'Adults_Only_Undefined_Type' -- Should be rare based on the outer WHEN condition
                   END
           END
       ELSE
           -- Customer does NOT buy Adult Apparel/Footwear etc.
           -- Check if they buy for Kids/Toys using the super category sum
           CASE
               WHEN Kids_Toys_REV_SUM > 0 THEN
                   -- They buy Kids/Toys items, but NOT Adult items --> Family: Kids Only
                   -- Now, infer kid age/gender mix (Kids Only)
                    CASE
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) AND (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) AND (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Kids Only_With_Young_and_Older_Mixed_Gender'
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) AND (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) THEN 'Family_Kids_Only_With_Young_and_Older_Boys'
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) AND (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Kids Only_With_Young_and_Older_Girls'
                       WHEN (Infant_FLAG = 1 OR Toddler_FLAG = 1) THEN 'Family_Kids_Only_With_Only_Young_Children'
                       WHEN (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) AND (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Kids_Only_With_Only_Older_Mixed_Gender'
                       WHEN (Boys_Junior_FLAG = 1 OR Boys_Senior_FLAG = 1) THEN 'Family_Kids_Only_With_Only_Older_Boys'
                       WHEN (Girls_Junior_FLAG = 1 OR Girls_Senior_FLAG = 1) THEN 'Family_Kids_Only_With_Only_Older_Girls'
                       -- Fallback for general Kids/Toys purchases when no Adults are present, but no *specific* age/gender flags triggered
                       ELSE 'Family_Kids_Only_With_Kids_or_Toys_Age_or_Gender_Unknown'
                    END
               ELSE
                   -- They buy NEITHER Adult items NOR Kids/Toys items --> Household/Other Only
                   'Household_or_Other_Only'
           END
   END AS Inferred_Household_Structure
FROM temp_db.an_clustering_model_customer_features_combined_2326;
