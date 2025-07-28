-- Advanced Field Specifications Example
-- Demonstrates all the advanced SQL*Loader field specifications
-- Generated on: 2025-01-27 15:30:00

LOAD DATA
INFILE 'advanced_data.csv'
BADFILE 'advanced_data.bad'
DISCARDFILE 'advanced_data.dsc'
APPEND
INTO TABLE advanced_example
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    -- Example 1: Field with custom terminator
    custom_field VARCHAR2(50) TERMINATED BY '|',
    
    -- Example 2: Field with advanced NULL handling
    status_field CHAR(3) NULLIF status_field = '999' OR status_field = '000',
    
    -- Example 3: Field with CASE transformation
    status_code VARCHAR2(10) "CASE WHEN :status_code = 'A' THEN 'ACTIVE' WHEN :status_code = 'I' THEN 'INACTIVE' ELSE 'UNKNOWN' END",
    
    -- Example 4: Field with DECODE transformation
    gender VARCHAR2(6) "DECODE(:gender, 'M', 'MALE', 'F', 'FEMALE', 'UNKNOWN')",
    
    -- Example 5: Field with validation rule
    age NUMBER(3) CHECK (age >= 0 AND age <= 150),
    
    -- Example 6: Field with field-level enclosure
    description VARCHAR2(200) OPTIONALLY ENCLOSED BY '"',
    
    -- Example 7: Field with character set
    unicode_text VARCHAR2(100) CHARACTERSET UTF8,
    
    -- Example 8: Field with complex default condition
    priority VARCHAR2(10) DEFAULTIF priority = BLANKS OR priority = '0' "NORMAL",
    
    -- Example 9: Field with LOB specification
    large_text CLOB(1000) TERMINATED BY EOF,
    
    -- Example 10: Field with binary data
    binary_data RAW(16) TERMINATED BY WHITESPACE,
    
    -- Example 11: Field with packed decimal
    amount DECIMAL EXTERNAL,
    
    -- Example 12: Field with binary integer
    binary_int BINARY_INTEGER,
    
    -- Example 13: Field with complex transformation
    calculated_field "CASE WHEN LENGTH(:calculated_field) < 3 THEN 'INVALID' ELSE :calculated_field END",
    
    -- Example 14: Field with multiple NULL conditions
    complex_field CHAR(10) NULLIF complex_field = '999' AND other_field = 'N' OR complex_field = '000',
    
    -- Example 15: Field with validation expression
    validated_field VARCHAR2(50) "CASE WHEN :validated_field NOT IN ('A', 'B', 'C') THEN 'INVALID' ELSE :validated_field END"
)

-- Example of fixed-width format with advanced specifications
INFILE 'fixed_width_advanced.txt'
BADFILE 'fixed_width_advanced.bad'
DISCARDFILE 'fixed_width_advanced.dsc'
APPEND
INTO TABLE fixed_width_advanced
(
    -- Example 16: Field with relative positioning
    relative_field POSITION(*+1) CHAR(10),
    
    -- Example 17: Field with alternative positions
    alt_field POSITION(1:10) CHAR(10) POSITION(15:25) CHAR(10),
    
    -- Example 18: Field with overlapping positions
    overlap_field1 POSITION(1:20) CHAR(20),
    overlap_field2 POSITION(10:30) CHAR(21),
    
    -- Example 19: Field with time zone format
    timestamp_field POSITION(31:50) TIMESTAMP "YYYY-MM-DD HH24:MI:SS TZH:TZM",
    
    -- Example 20: Field with Julian date format
    julian_date POSITION(51:57) DATE "YYYYDDD",
    
    -- Example 21: Field with binary float
    binary_float_field POSITION(58:65) BINARY_FLOAT,
    
    -- Example 22: Field with binary double
    binary_double_field POSITION(66:81) BINARY_DOUBLE,
    
    -- Example 23: Field with zoned decimal
    zoned_decimal POSITION(82:90) ZONED DECIMAL,
    
    -- Example 24: Field with complex CASE statement
    complex_case POSITION(91:100) CHAR(10) "CASE 
        WHEN :complex_case = 'A' THEN 'ACTIVE'
        WHEN :complex_case = 'I' THEN 'INACTIVE'
        WHEN :complex_case = 'D' THEN 'DELETED'
        WHEN :complex_case = 'P' THEN 'PENDING'
        ELSE 'UNKNOWN'
    END",
    
    -- Example 25: Field with DECODE and multiple values
    multi_decode POSITION(101:110) CHAR(10) "DECODE(:multi_decode, '1', 'ONE', '2', 'TWO', '3', 'THREE', 'DEFAULT')"
) 