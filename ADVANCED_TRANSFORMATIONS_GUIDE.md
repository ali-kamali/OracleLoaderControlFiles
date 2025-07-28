# Advanced Transformations Guide

## Overview

This guide documents the advanced SQL transformations and functions implemented in the Oracle SQL*Loader Control File Generator. These transformations enable complex data processing during the loading process, allowing you to clean, validate, format, and transform data as it's loaded into Oracle databases.

## Table of Contents

1. [String Transformations](#string-transformations)
2. [Date Transformations](#date-transformations)
3. [Mathematical Transformations](#mathematical-transformations)
4. [Conditional Transformations](#conditional-transformations)
5. [Type Conversion Transformations](#type-conversion-transformations)
6. [Aggregate Transformations](#aggregate-transformations)
7. [Complex Combined Transformations](#complex-combined-transformations)
8. [Validation Transformations](#validation-transformations)
9. [Business Logic Transformations](#business-logic-transformations)
10. [Data Cleansing Transformations](#data-cleansing-transformations)
11. [Formatting Transformations](#formatting-transformations)
12. [Null Handling Transformations](#null-handling-transformations)
13. [Configuration and Parameters](#configuration-and-parameters)
14. [Best Practices](#best-practices)

## String Transformations

### Overview
String transformations allow you to manipulate text data during loading.

### Available Functions

#### UPPER
Converts text to uppercase.

```sql
field1 VARCHAR2(50) "UPPER(:field1)"
```

#### LOWER
Converts text to lowercase.

```sql
field1 VARCHAR2(50) "LOWER(:field1)"
```

#### TRIM
Removes leading and trailing whitespace.

```sql
field1 VARCHAR2(100) "TRIM(:field1)"
```

#### LTRIM
Removes leading whitespace.

```sql
field1 VARCHAR2(100) "LTRIM(:field1)"
```

#### RTRIM
Removes trailing whitespace.

```sql
field1 VARCHAR2(100) "RTRIM(:field1)"
```

#### SUBSTR
Extracts a substring from a string.

```sql
-- Extract from position 1, length 20
field1 VARCHAR2(50) "SUBSTR(:field1, 1, 20)"

-- Extract from position 5 to end
field1 VARCHAR2(50) "SUBSTR(:field1, 5)"
```

#### REPLACE
Replaces occurrences of a substring.

```sql
field1 VARCHAR2(100) "REPLACE(:field1, 'old', 'new')"
```

#### CONCAT
Concatenates strings.

```sql
field1 VARCHAR2(100) "CONCAT(:field1, '_SUFFIX')"
```

#### LENGTH
Returns the length of a string.

```sql
field1 NUMBER(3) "LENGTH(:field1)"
```

#### INSTR
Finds the position of a substring.

```sql
-- Find 'search' starting from position 1, first occurrence
field1 NUMBER(3) "INSTR(:field1, 'search', 1, 1)"
```

#### LPAD
Left-pads a string to a specified length.

```sql
field1 VARCHAR2(20) "LPAD(:field1, 10, '0')"
```

#### RPAD
Right-pads a string to a specified length.

```sql
field1 VARCHAR2(20) "RPAD(:field1, 10, ' ')"
```

#### REGEXP_REPLACE
Replaces text using regular expressions.

```sql
-- Replace all numbers with 'NUMBER'
field1 VARCHAR2(100) "REGEXP_REPLACE(:field1, '[0-9]+', 'NUMBER', 1, 1)"

-- Remove all non-alphanumeric characters
field1 VARCHAR2(200) "REGEXP_REPLACE(:field1, '[^A-Za-z0-9 ]', '', 1, 0)"
```

#### REGEXP_SUBSTR
Extracts text using regular expressions.

```sql
-- Extract first sequence of uppercase letters
field1 VARCHAR2(50) "REGEXP_SUBSTR(:field1, '[A-Z]+', 1, 1)"
```

### Configuration
- **TransformType**: "STRING"
- **TransformParameters**: Function name and parameters (e.g., "UPPER", "SUBSTR,1,20", "REPLACE,old,new")

## Date Transformations

### Overview
Date transformations allow you to convert, format, and manipulate date/time data.

### Available Functions

#### TO_DATE
Converts string to date.

```sql
field1 DATE "TO_DATE(:field1, 'YYYY-MM-DD')"
field2 DATE "TO_DATE(:field2, 'YYYYMMDD')"
```

#### TO_TIMESTAMP
Converts string to timestamp.

```sql
field1 TIMESTAMP "TO_TIMESTAMP(:field1, 'YYYY-MM-DD HH24:MI:SS')"
field2 TIMESTAMP "TO_TIMESTAMP(:field2, 'YYYYMMDDHH24MISS')"
```

#### TO_CHAR
Converts date to formatted string.

```sql
field1 VARCHAR2(20) "TO_CHAR(:field1, 'YYYY-MM-DD')"
field2 VARCHAR2(30) "TO_CHAR(:field2, '999,999.99')"
```

#### ADD_MONTHS
Adds months to a date.

```sql
field1 DATE "ADD_MONTHS(:field1, 3)"
```

#### MONTHS_BETWEEN
Calculates months between two dates.

```sql
field1 NUMBER(5,2) "MONTHS_BETWEEN(:field1, SYSDATE)"
```

#### NEXT_DAY
Finds the next occurrence of a day of the week.

```sql
field1 DATE "NEXT_DAY(:field1, 'MONDAY')"
```

#### LAST_DAY
Finds the last day of the month.

```sql
field1 DATE "LAST_DAY(:field1)"
```

#### TRUNC
Truncates a date to a specified unit.

```sql
field1 DATE "TRUNC(:field1, 'MM')"  -- First day of month
field2 DATE "TRUNC(:field1, 'YYYY')" -- First day of year
```

#### ROUND
Rounds a date to a specified unit.

```sql
field1 DATE "ROUND(:field1, 'YYYY')"
```

#### EXTRACT
Extracts components from a date.

```sql
field1 NUMBER(4) "EXTRACT(YEAR FROM :field1)"
field2 NUMBER(2) "EXTRACT(MONTH FROM :field2)"
field3 NUMBER(2) "EXTRACT(DAY FROM :field3)"
```

#### SYSDATE
Returns current system date.

```sql
field1 DATE "SYSDATE"
```

#### SYSTIMESTAMP
Returns current system timestamp.

```sql
field1 TIMESTAMP "SYSTIMESTAMP"
```

### Configuration
- **TransformType**: "DATE"
- **TransformParameters**: Function name and format (e.g., "TO_DATE,YYYY-MM-DD", "ADD_MONTHS,3")

## Mathematical Transformations

### Overview
Mathematical transformations allow you to perform calculations on numeric data.

### Available Functions

#### Basic Arithmetic
```sql
field1 NUMBER(10,2) ":field1 * 100"
field2 NUMBER(5) ":field2 + 1"
field3 NUMBER(8,2) ":field3 / 100"
```

#### Mathematical Functions
```sql
field1 NUMBER(5,2) "MOD(:field1, 100)"
field2 NUMBER(10,2) "POWER(:field2, 2)"
field3 NUMBER(10,4) "SQRT(:field3)"
field4 NUMBER(8,2) "ABS(:field4)"
field5 NUMBER(8,2) "ROUND(:field5, 2)"
field6 NUMBER(8,2) "TRUNC(:field6, 2)"
field7 NUMBER(8,2) "CEIL(:field7)"
field8 NUMBER(8,2) "FLOOR(:field8)"
```

### Configuration
- **TransformType**: "MATHEMATICAL"
- **TransformParameters**: Operation and operand (e.g., "*,100", "ROUND,2")

## Conditional Transformations

### Overview
Conditional transformations allow you to apply different logic based on conditions.

### CASE Statements
```sql
-- Simple CASE
field1 VARCHAR2(10) "CASE WHEN :field1 = 'A' THEN 'ACTIVE' WHEN :field1 = 'I' THEN 'INACTIVE' ELSE 'UNKNOWN' END"

-- Complex CASE with multiple conditions
field1 VARCHAR2(20) "CASE WHEN LENGTH(:field1) < 3 THEN 'SHORT' WHEN LENGTH(:field1) < 10 THEN 'MEDIUM' ELSE 'LONG' END"

-- NULL handling
field1 VARCHAR2(20) "CASE WHEN :field1 IS NOT NULL THEN 'HAS_VALUE' ELSE 'NO_VALUE' END"
```

### DECODE Functions
```sql
-- Simple DECODE
field1 VARCHAR2(6) "DECODE(:field1, 'M', 'MALE', 'F', 'FEMALE', 'UNKNOWN')"

-- Multiple values
field1 VARCHAR2(10) "DECODE(:field1, '1', 'HIGH', '2', 'MEDIUM', '3', 'LOW', 'DEFAULT')"
```

### Configuration
- **TransformType**: "CASE" or "DECODE"
- **TransformParameters**: 
  - CASE: "WHEN condition1 THEN result1,WHEN condition2 THEN result2,ELSE default"
  - DECODE: "value1,result1,value2,result2,default"

## Type Conversion Transformations

### Overview
Type conversion transformations allow you to convert data between different types.

### Available Functions

#### TO_NUMBER
Converts string to number.

```sql
field1 NUMBER(8,2) "TO_NUMBER(:field1, '999999.99')"
```

#### TO_CHAR
Converts number to formatted string.

```sql
field1 VARCHAR2(20) "TO_CHAR(:field1, '999999.99')"
```

#### CAST
Casts to a specific data type.

```sql
field1 VARCHAR2(50) "CAST(:field1 AS VARCHAR2(50))"
```

#### CONVERT
Converts character set.

```sql
field1 VARCHAR2(30) "CONVERT(:field1, 'UTF8')"
```

### Configuration
- **TransformType**: "TYPE_CONVERSION"
- **TransformParameters**: Function and format (e.g., "TO_NUMBER,999999.99", "CAST,VARCHAR2(50)")

## Aggregate Transformations

### Overview
Aggregate transformations allow you to perform calculations across multiple rows.

### Available Functions
```sql
field1 NUMBER(10,2) "SUM(:field1)"
field2 NUMBER(8,2) "AVG(:field2)"
field3 NUMBER(5) "COUNT(:field3)"
field4 VARCHAR2(50) "MAX(:field4)"
field5 VARCHAR2(50) "MIN(:field5)"
```

### Configuration
- **TransformType**: "AGGREGATE"
- **TransformParameters**: Function name (e.g., "SUM", "AVG", "COUNT")

## Complex Combined Transformations

### Overview
Complex transformations combine multiple functions for sophisticated data processing.

### Examples

#### String and Conditional
```sql
field1 VARCHAR2(100) "CASE WHEN LENGTH(:field1) < 5 THEN UPPER(:field1) ELSE LOWER(:field1) END"
```

#### Mathematical and Conditional
```sql
field1 NUMBER(10,2) "CASE WHEN :field1 > 1000 THEN :field1 * 0.9 ELSE :field1 * 1.1 END"
```

#### String and DECODE
```sql
field1 VARCHAR2(50) "DECODE(SUBSTR(:field1, 1, 1), 'A', 'ALPHA', 'N', 'NUMERIC', 'S', 'SPECIAL', 'OTHER')"
```

#### Date and Conditional
```sql
field1 DATE "CASE WHEN :field1 IS NULL THEN SYSDATE ELSE ADD_MONTHS(:field1, 1) END"
```

## Validation Transformations

### Overview
Validation transformations ensure data quality and compliance with business rules.

### Examples

#### Value Validation
```sql
field1 VARCHAR2(50) "CASE WHEN :field1 NOT IN ('A', 'B', 'C') THEN 'INVALID' ELSE :field1 END"
```

#### Range Validation
```sql
field1 NUMBER(8,2) "CASE WHEN :field1 < 0 OR :field1 > 10000 THEN 0 ELSE :field1 END"
```

#### Length Validation
```sql
field1 VARCHAR2(30) "CASE WHEN LENGTH(:field1) < 3 OR LENGTH(:field1) > 20 THEN 'INVALID_LENGTH' ELSE :field1 END"
```

## Business Logic Transformations

### Overview
Business logic transformations implement specific business rules and calculations.

### Examples

#### Status Mapping
```sql
field1 VARCHAR2(20) "CASE WHEN :field1 = 'Y' THEN 'YES' WHEN :field1 = 'N' THEN 'NO' WHEN :field1 = 'U' THEN 'UNKNOWN' ELSE 'INVALID' END"
```

#### Tiered Calculations
```sql
field1 NUMBER(8,2) "CASE WHEN :field1 <= 100 THEN :field1 * 1.05 WHEN :field1 <= 500 THEN :field1 * 1.10 WHEN :field1 <= 1000 THEN :field1 * 1.15 ELSE :field1 * 1.20 END"
```

#### Customer Classification
```sql
field1 VARCHAR2(15) "DECODE(:field1, 'NEW', 'NEW_CUSTOMER', 'EXISTING', 'EXISTING_CUSTOMER', 'INACTIVE', 'INACTIVE_CUSTOMER', 'UNKNOWN')"
```

## Data Cleansing Transformations

### Overview
Data cleansing transformations clean and standardize data.

### Examples

#### Remove Tabs and Trim
```sql
field1 VARCHAR2(100) "TRIM(REPLACE(:field1, CHR(9), ' '))"
```

#### Standardize Case
```sql
field1 VARCHAR2(50) "UPPER(TRIM(:field1))"
```

#### Remove Special Characters
```sql
field1 VARCHAR2(200) "REGEXP_REPLACE(:field1, '[^A-Za-z0-9 ]', '', 1, 0)"
```

## Formatting Transformations

### Overview
Formatting transformations format data for display or storage.

### Examples

#### Zero-Pad Numbers
```sql
field1 VARCHAR2(20) "LPAD(TO_CHAR(:field1), 8, '0')"
```

#### Format Numbers with Commas
```sql
field1 VARCHAR2(30) "TO_CHAR(:field1, '999,999.99')"
```

#### Create Formatted IDs
```sql
field1 VARCHAR2(50) "CONCAT('ID-', LPAD(TO_CHAR(:field1), 6, '0'))"
```

## Null Handling Transformations

### Overview
Null handling transformations provide default values for null data.

### Examples

#### NVL Function
```sql
field1 VARCHAR2(50) "NVL(:field1, 'DEFAULT')"
field2 NUMBER(8,2) "NVL(:field2, 0)"
field3 DATE "NVL(:field3, SYSDATE)"
```

#### COALESCE Function
```sql
field1 VARCHAR2(100) "COALESCE(:field1, 'DEFAULT1', 'DEFAULT2', 'DEFAULT3')"
```

## Configuration and Parameters

### FieldDefinition Properties

#### TransformType
Specifies the type of transformation to apply:
- "STRING" - String manipulation functions
- "DATE" - Date/time functions
- "MATHEMATICAL" - Mathematical operations
- "CASE" - CASE statements
- "DECODE" - DECODE functions
- "FUNCTION" - Direct SQL functions
- "CONDITIONAL" - Conditional logic
- "AGGREGATE" - Aggregate functions
- "TYPE_CONVERSION" - Type conversion functions
- "CUSTOM" - Custom SQL expressions

#### TransformParameters
Comma-separated parameters for the transformation:
- String functions: "UPPER", "SUBSTR,1,20", "REPLACE,old,new"
- Date functions: "TO_DATE,YYYY-MM-DD", "ADD_MONTHS,3"
- Mathematical: "*,100", "ROUND,2"
- CASE: "WHEN A THEN ACTIVE,WHEN I THEN INACTIVE,ELSE UNKNOWN"
- DECODE: "M,MALE,F,FEMALE,UNKNOWN"

#### ConditionalTransform
Direct SQL expression for complex transformations:
```sql
"CASE WHEN LENGTH(:field1) < 3 THEN 'SHORT' ELSE 'LONG' END"
```

### Usage in Control File Generator

The AdvancedTransformationService automatically generates the appropriate SQL transformation based on the field configuration:

```csharp
var transformationService = new AdvancedTransformationService();
var transformation = transformationService.GenerateAdvancedTransformation(field);
```

## Best Practices

### 1. Performance Considerations
- Use simple transformations when possible
- Avoid complex nested functions for large datasets
- Consider using database functions for complex logic
- Test transformation performance with sample data

### 2. Data Quality
- Always validate transformation results
- Use appropriate error handling
- Test with edge cases and null values
- Document complex transformation logic

### 3. Maintainability
- Use descriptive field names
- Document transformation parameters
- Create reusable transformation templates
- Version control transformation configurations

### 4. Error Handling
- Provide default values for failed transformations
- Use CASE statements to handle edge cases
- Validate input data before transformation
- Log transformation errors

### 5. Testing
- Test with various data types and formats
- Verify transformation results manually
- Test with null and empty values
- Validate performance with large datasets

## Examples by Category

### String Processing
```sql
-- Clean and standardize names
name VARCHAR2(100) "UPPER(TRIM(:name))"

-- Extract domain from email
domain VARCHAR2(50) "SUBSTR(:email, INSTR(:email, '@') + 1)"

-- Remove special characters
clean_text VARCHAR2(200) "REGEXP_REPLACE(:text, '[^A-Za-z0-9 ]', '', 1, 0)"
```

### Date Processing
```sql
-- Convert various date formats
standard_date DATE "TO_DATE(:date_field, 'YYYY-MM-DD')"

-- Calculate age
age NUMBER(3) "EXTRACT(YEAR FROM SYSDATE) - EXTRACT(YEAR FROM :birth_date)"

-- Business days (simplified)
business_date DATE "CASE WHEN TO_CHAR(:date_field, 'D') IN ('1', '7') THEN NEXT_DAY(:date_field, 'MONDAY') ELSE :date_field END"
```

### Numeric Processing
```sql
-- Currency conversion
usd_amount NUMBER(10,2) ":amount * 1.15"

-- Tiered pricing
final_price NUMBER(8,2) "CASE WHEN :quantity <= 10 THEN :price WHEN :quantity <= 50 THEN :price * 0.95 ELSE :price * 0.90 END"

-- Round to nearest quarter
quarter_amount NUMBER(8,2) "ROUND(:amount * 4) / 4"
```

### Business Logic
```sql
-- Customer status
status VARCHAR2(20) "CASE WHEN :last_purchase_date < SYSDATE - 365 THEN 'INACTIVE' WHEN :total_purchases > 1000 THEN 'PREMIUM' ELSE 'ACTIVE' END"

-- Risk assessment
risk_level VARCHAR2(10) "CASE WHEN :credit_score < 600 THEN 'HIGH' WHEN :credit_score < 700 THEN 'MEDIUM' ELSE 'LOW' END"

-- Product category
category VARCHAR2(15) "DECODE(SUBSTR(:product_code, 1, 2), 'EL', 'ELECTRONICS', 'CL', 'CLOTHING', 'BK', 'BOOKS', 'OTHER')"
```

## Conclusion

Advanced transformations provide powerful capabilities for data processing during the loading process. By leveraging these features, you can ensure data quality, implement business rules, and create standardized data formats without requiring additional processing steps.

For more information, refer to the Oracle SQL documentation and the example control files provided with this application. 