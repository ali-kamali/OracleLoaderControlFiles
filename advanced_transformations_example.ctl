-- Advanced Transformations Example
-- Demonstrates complex SQL transformations and functions in Oracle SQL*Loader
-- Generated on: 2025-01-27 16:00:00

LOAD DATA
INFILE 'advanced_transformations.csv'
BADFILE 'advanced_transformations.bad'
DISCARDFILE 'advanced_transformations.dsc'
APPEND
INTO TABLE advanced_transformations
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    -- Basic field (no transformation)
    id NUMBER(6) INTEGER EXTERNAL,
    
    -- String Transformations
    name VARCHAR2(50) "UPPER(:name)",
    description VARCHAR2(200) "LOWER(:description)",
    text_field VARCHAR2(100) "TRIM(:text_field)",
    substring_field VARCHAR2(50) "SUBSTR(:substring_field, 1, 20)",
    replace_field VARCHAR2(100) "REPLACE(:replace_field, 'old', 'new')",
    concat_field VARCHAR2(100) "CONCAT(:concat_field, '_SUFFIX')",
    length_field NUMBER(3) "LENGTH(:length_field)",
    instr_field NUMBER(3) "INSTR(:instr_field, 'search', 1, 1)",
    lpad_field VARCHAR2(20) "LPAD(:lpad_field, 10, '0')",
    rpad_field VARCHAR2(20) "RPAD(:rpad_field, 10, ' ')",
    regexp_replace_field VARCHAR2(100) "REGEXP_REPLACE(:regexp_replace_field, '[0-9]+', 'NUMBER', 1, 1)",
    regexp_substr_field VARCHAR2(50) "REGEXP_SUBSTR(:regexp_substr_field, '[A-Z]+', 1, 1)",
    
    -- Date Transformations
    date_field DATE "TO_DATE(:date_field, 'YYYY-MM-DD')",
    timestamp_field TIMESTAMP "TO_TIMESTAMP(:timestamp_field, 'YYYY-MM-DD HH24:MI:SS')",
    char_date VARCHAR2(20) "TO_CHAR(:char_date, 'YYYY-MM-DD')",
    add_months_field DATE "ADD_MONTHS(:add_months_field, 3)",
    months_between_field NUMBER(5,2) "MONTHS_BETWEEN(:months_between_field, SYSDATE)",
    next_day_field DATE "NEXT_DAY(:next_day_field, 'MONDAY')",
    last_day_field DATE "LAST_DAY(:last_day_field)",
    trunc_date_field DATE "TRUNC(:trunc_date_field, 'MM')",
    round_date_field DATE "ROUND(:round_date_field, 'YYYY')",
    extract_year_field NUMBER(4) "EXTRACT(YEAR FROM :extract_year_field)",
    extract_month_field NUMBER(2) "EXTRACT(MONTH FROM :extract_month_field)",
    extract_day_field NUMBER(2) "EXTRACT(DAY FROM :extract_day_field)",
    sysdate_field DATE "SYSDATE",
    systimestamp_field TIMESTAMP "SYSTIMESTAMP",
    
    -- Mathematical Transformations
    amount NUMBER(10,2) ":amount * 100",
    quantity NUMBER(5) ":quantity + 1",
    price NUMBER(8,2) ":price / 100",
    discount NUMBER(5,2) "MOD(:discount, 100)",
    power_field NUMBER(10,2) "POWER(:power_field, 2)",
    sqrt_field NUMBER(10,4) "SQRT(:sqrt_field)",
    abs_field NUMBER(8,2) "ABS(:abs_field)",
    round_field NUMBER(8,2) "ROUND(:round_field, 2)",
    trunc_field NUMBER(8,2) "TRUNC(:trunc_field, 2)",
    ceil_field NUMBER(8,2) "CEIL(:ceil_field)",
    floor_field NUMBER(8,2) "FLOOR(:floor_field)",
    
    -- Conditional Transformations
    status_field VARCHAR2(10) "CASE WHEN :status_field = 'A' THEN 'ACTIVE' WHEN :status_field = 'I' THEN 'INACTIVE' ELSE 'UNKNOWN' END",
    conditional_field VARCHAR2(20) "CASE WHEN :conditional_field IS NOT NULL THEN 'HAS_VALUE' ELSE 'NO_VALUE' END",
    complex_conditional VARCHAR2(20) "CASE WHEN LENGTH(:complex_conditional) < 3 THEN 'SHORT' WHEN LENGTH(:complex_conditional) < 10 THEN 'MEDIUM' ELSE 'LONG' END",
    
    -- DECODE Transformations
    gender_field VARCHAR2(6) "DECODE(:gender_field, 'M', 'MALE', 'F', 'FEMALE', 'UNKNOWN')",
    priority_field VARCHAR2(10) "DECODE(:priority_field, '1', 'HIGH', '2', 'MEDIUM', '3', 'LOW', 'DEFAULT')",
    category_field VARCHAR2(15) "DECODE(:category_field, 'A', 'ADMIN', 'U', 'USER', 'G', 'GUEST', 'UNKNOWN')",
    
    -- Type Conversion Transformations
    numeric_string NUMBER(8,2) "TO_NUMBER(:numeric_string, '999999.99')",
    char_number VARCHAR2(20) "TO_CHAR(:char_number, '999999.99')",
    date_string DATE "TO_DATE(:date_string, 'YYYYMMDD')",
    timestamp_string TIMESTAMP "TO_TIMESTAMP(:timestamp_string, 'YYYY-MM-DD HH24:MI:SS')",
    cast_field VARCHAR2(50) "CAST(:cast_field AS VARCHAR2(50))",
    convert_field VARCHAR2(30) "CONVERT(:convert_field, 'UTF8')",
    
    -- Aggregate Transformations
    sum_field NUMBER(10,2) "SUM(:sum_field)",
    avg_field NUMBER(8,2) "AVG(:avg_field)",
    count_field NUMBER(5) "COUNT(:count_field)",
    max_field VARCHAR2(50) "MAX(:max_field)",
    min_field VARCHAR2(50) "MIN(:min_field)",
    
    -- Complex Combined Transformations
    complex_field1 VARCHAR2(100) "CASE WHEN LENGTH(:complex_field1) < 5 THEN UPPER(:complex_field1) ELSE LOWER(:complex_field1) END",
    complex_field2 NUMBER(10,2) "CASE WHEN :complex_field2 > 1000 THEN :complex_field2 * 0.9 ELSE :complex_field2 * 1.1 END",
    complex_field3 VARCHAR2(50) "DECODE(SUBSTR(:complex_field3, 1, 1), 'A', 'ALPHA', 'N', 'NUMERIC', 'S', 'SPECIAL', 'OTHER')",
    complex_field4 DATE "CASE WHEN :complex_field4 IS NULL THEN SYSDATE ELSE ADD_MONTHS(:complex_field4, 1) END",
    
    -- Validation Transformations
    validated_field1 VARCHAR2(50) "CASE WHEN :validated_field1 NOT IN ('A', 'B', 'C') THEN 'INVALID' ELSE :validated_field1 END",
    validated_field2 NUMBER(8,2) "CASE WHEN :validated_field2 < 0 OR :validated_field2 > 10000 THEN 0 ELSE :validated_field2 END",
    validated_field3 VARCHAR2(30) "CASE WHEN LENGTH(:validated_field3) < 3 OR LENGTH(:validated_field3) > 20 THEN 'INVALID_LENGTH' ELSE :validated_field3 END",
    
    -- Business Logic Transformations
    business_field1 VARCHAR2(20) "CASE WHEN :business_field1 = 'Y' THEN 'YES' WHEN :business_field1 = 'N' THEN 'NO' WHEN :business_field1 = 'U' THEN 'UNKNOWN' ELSE 'INVALID' END",
    business_field2 NUMBER(8,2) "CASE WHEN :business_field2 <= 100 THEN :business_field2 * 1.05 WHEN :business_field2 <= 500 THEN :business_field2 * 1.10 WHEN :business_field2 <= 1000 THEN :business_field2 * 1.15 ELSE :business_field2 * 1.20 END",
    business_field3 VARCHAR2(15) "DECODE(:business_field3, 'NEW', 'NEW_CUSTOMER', 'EXISTING', 'EXISTING_CUSTOMER', 'INACTIVE', 'INACTIVE_CUSTOMER', 'UNKNOWN')",
    
    -- Data Cleansing Transformations
    clean_field1 VARCHAR2(100) "TRIM(REPLACE(:clean_field1, CHR(9), ' '))",
    clean_field2 VARCHAR2(50) "UPPER(TRIM(:clean_field2))",
    clean_field3 VARCHAR2(200) "REGEXP_REPLACE(:clean_field3, '[^A-Za-z0-9 ]', '', 1, 0)",
    
    -- Formatting Transformations
    format_field1 VARCHAR2(20) "LPAD(TO_CHAR(:format_field1), 8, '0')",
    format_field2 VARCHAR2(30) "TO_CHAR(:format_field2, '999,999.99')",
    format_field3 VARCHAR2(50) "CONCAT('ID-', LPAD(TO_CHAR(:format_field3), 6, '0'))",
    
    -- Null Handling Transformations
    null_field1 VARCHAR2(50) "NVL(:null_field1, 'DEFAULT')",
    null_field2 NUMBER(8,2) "NVL(:null_field2, 0)",
    null_field3 DATE "NVL(:null_field3, SYSDATE)",
    coalesce_field VARCHAR2(100) "COALESCE(:coalesce_field, 'DEFAULT1', 'DEFAULT2', 'DEFAULT3')"
)

-- Fixed-width format with advanced transformations
INFILE 'fixed_width_transformations.txt'
BADFILE 'fixed_width_transformations.bad'
DISCARDFILE 'fixed_width_transformations.dsc'
APPEND
INTO TABLE fixed_width_transformations
(
    -- Fixed-width fields with transformations
    id_field POSITION(1:6) NUMBER(6) INTEGER EXTERNAL,
    name_field POSITION(7:36) VARCHAR2(30) "UPPER(:name_field)",
    amount_field POSITION(37:46) NUMBER(10,2) ":amount_field * 100",
    date_field POSITION(47:54) DATE "TO_DATE(:date_field, 'YYYYMMDD')",
    status_field POSITION(55:55) CHAR(1) "CASE WHEN :status_field = 'A' THEN 'ACTIVE' WHEN :status_field = 'I' THEN 'INACTIVE' ELSE 'UNKNOWN' END",
    description_field POSITION(56:105) VARCHAR2(50) "TRIM(:description_field)",
    numeric_field POSITION(106:115) NUMBER(10) "ROUND(:numeric_field, 2)",
    timestamp_field POSITION(116:133) TIMESTAMP "TO_TIMESTAMP(:timestamp_field, 'YYYYMMDDHH24MISS')",
    category_field POSITION(134:143) VARCHAR2(10) "DECODE(:category_field, '1', 'CATEGORY1', '2', 'CATEGORY2', '3', 'CATEGORY3', 'OTHER')",
    validation_field POSITION(144:153) VARCHAR2(10) "CASE WHEN LENGTH(:validation_field) < 3 THEN 'INVALID' ELSE :validation_field END"
) 