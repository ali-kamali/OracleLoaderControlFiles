# Advanced Field Specifications Guide

## Overview

This guide documents the advanced field specifications that have been implemented in the Oracle SQL*Loader Control File Generator. These features enable complex data loading scenarios that are common in enterprise environments.

## Table of Contents

1. [Field-Level Terminators](#field-level-terminators)
2. [Field-Level Enclosures](#field-level-enclosures)
3. [Advanced Data Types](#advanced-data-types)
4. [Advanced NULL Handling](#advanced-null-handling)
5. [Advanced Transformations](#advanced-transformations)
6. [Advanced Positioning](#advanced-positioning)
7. [Character Set Support](#character-set-support)
8. [Field Validation](#field-validation)
9. [Advanced Date/Time Handling](#advanced-datetime-handling)
10. [Advanced Numeric Handling](#advanced-numeric-handling)
11. [Usage Examples](#usage-examples)
12. [Best Practices](#best-practices)

## Field-Level Terminators

### Overview
Field-level terminators allow you to specify different delimiters for individual fields, overriding the global field terminator.

### Supported Terminator Types

#### CHARACTER
Specifies a specific character as the field terminator.

```sql
field1 VARCHAR2(50) TERMINATED BY '|',
field2 CHAR(20) TERMINATED BY ';',
field3 NUMBER TERMINATED BY '#'
```

#### WHITESPACE
Uses whitespace (spaces, tabs) as the field terminator.

```sql
field1 VARCHAR2(50) TERMINATED BY WHITESPACE,
field2 CHAR(20) TERMINATED BY WHITESPACE
```

#### EOF
Uses end-of-file as the field terminator (commonly used for LOB fields).

```sql
lob_field CLOB(1000) TERMINATED BY EOF,
large_text BLOB(2000) TERMINATED BY EOF
```

### Configuration
- **FieldTerminator**: The character or keyword to use as terminator
- **TerminatorType**: CHARACTER, WHITESPACE, or EOF

## Field-Level Enclosures

### Overview
Field-level enclosures allow you to specify different enclosure characters for individual fields.

### Supported Enclosure Types

#### ENCLOSED BY
Fields must be enclosed by the specified character.

```sql
field1 VARCHAR2(100) ENCLOSED BY '"',
field2 CHAR(50) ENCLOSED BY "'"
```

#### OPTIONALLY ENCLOSED BY
Fields may be enclosed by the specified character.

```sql
field1 VARCHAR2(100) OPTIONALLY ENCLOSED BY '"',
field2 CHAR(50) OPTIONALLY ENCLOSED BY "'"
```

### Configuration
- **FieldEnclosedBy**: The enclosure character
- **FieldOptionallyEnclosed**: Boolean flag for optional enclosure

## Advanced Data Types

### Overview
Support for complex Oracle data types with precision, scale, and modifiers.

### NUMBER Types with Precision and Scale

```sql
amount NUMBER(10,2) EXTERNAL,
quantity NUMBER(5) INTEGER EXTERNAL,
price DECIMAL EXTERNAL
```

### LOB Types

```sql
clob_field CLOB(1000) TERMINATED BY EOF,
blob_field BLOB(2000) TERMINATED BY EOF
```

### RAW Data

```sql
binary_field RAW(16) TERMINATED BY WHITESPACE,
hash_field RAW(32) TERMINATED BY ','
```

### Binary Types

```sql
binary_int BINARY_INTEGER,
binary_float BINARY_FLOAT,
binary_double BINARY_DOUBLE
```

### Packed Decimal

```sql
packed_field DECIMAL EXTERNAL,
zoned_field ZONED DECIMAL
```

### Configuration
- **Precision**: Number of digits (1-38)
- **Scale**: Number of decimal places (0-127)
- **LobSize**: Size for LOB fields
- **DataTypeModifier**: EXTERNAL, INTERNAL, BINARY_INTEGER, etc.

## Advanced NULL Handling

### Overview
Complex NULL condition specifications for conditional data loading.

### Basic NULLIF

```sql
field1 NULLIF field1 = BLANKS,
field2 NULLIF field2 = '999',
field3 NULLIF field3 = '0'
```

### Compound NULLIF

```sql
field1 NULLIF field1 = '999' AND field2 = 'N',
field2 NULLIF field2 = '000' OR field2 = '999'
```

### DEFAULTIF

```sql
field1 DEFAULTIF field1 = BLANKS 'UNKNOWN',
field2 DEFAULTIF field2 = '0' 'N/A'
```

### Complex Default Conditions

```sql
field1 DEFAULTIF field1 = BLANKS OR field1 = '0' 'DEFAULT_VALUE'
```

### Configuration
- **NullCondition**: Complex NULL condition expression
- **DefaultCondition**: Complex default condition expression
- **NullOperator**: AND or OR for compound conditions

## Advanced Transformations

### Overview
Complex SQL transformations for data processing during loading.

### CASE Statements

```sql
field1 "CASE 
    WHEN :field1 = 'A' THEN 'ACTIVE'
    WHEN :field1 = 'I' THEN 'INACTIVE'
    WHEN :field1 = 'D' THEN 'DELETED'
    ELSE 'UNKNOWN'
END"
```

### DECODE Functions

```sql
field1 "DECODE(:field1, 'M', 'MALE', 'F', 'FEMALE', 'UNKNOWN')",
field2 "DECODE(:field2, '1', 'ONE', '2', 'TWO', '3', 'THREE', 'DEFAULT')"
```

### SQL Functions

```sql
field1 "UPPER(:field1)",
field2 "LOWER(:field2)",
field3 "TRIM(:field3)",
field4 "SUBSTR(:field4, 1, 10)",
field5 "TO_DATE(:field5, 'YYYYMMDD')",
field6 "TO_NUMBER(:field6, '999999.99')"
```

### Mathematical Operations

```sql
field1 ":field1 * 100",
field2 "ROUND(:field2, 2)",
field3 "TRUNC(:field3)"
```

### Conditional Transformations

```sql
field1 "CASE WHEN LENGTH(:field1) < 3 THEN 'INVALID' ELSE :field1 END",
field2 "CASE WHEN :field2 NOT IN ('A', 'B', 'C') THEN 'INVALID' ELSE :field2 END"
```

### Configuration
- **TransformType**: CASE, DECODE, FUNCTION
- **TransformParameters**: Parameters for the transformation
- **ConditionalTransform**: Complex conditional transformation

## Advanced Positioning

### Overview
Advanced positioning options for fixed-width files.

### Relative Positioning

```sql
field1 POSITION(1:10),
field2 POSITION(*+1:20),  -- Start after previous field
field3 POSITION(21:30)
```

### Alternative Positions

```sql
field1 POSITION(1:10) CHAR(10),
field1 POSITION(15:25) CHAR(10)  -- Alternative position
```

### Overlapping Positions

```sql
field1 POSITION(1:20) CHAR(20),
field2 POSITION(10:30) CHAR(21)  -- Overlaps with field1
```

### Configuration
- **RelativePosition**: Boolean for relative positioning
- **AlternativePositions**: Comma-separated position ranges
- **PositionType**: ABSOLUTE, RELATIVE, OVERLAPPING

## Character Set Support

### Overview
Field-level character set specifications for international data.

### Supported Character Sets

```sql
field1 VARCHAR2(100) CHARACTERSET UTF8,
field2 CLOB CHARACTERSET AL32UTF8,
field3 VARCHAR2(50) CHARACTERSET ZHS16GBK,
field4 CHAR(20) CHARACTERSET AL16UTF16,
field5 VARCHAR2(30) CHARACTERSET WE8ISO8859P1
```

### Configuration
- **FieldCharacterSet**: Character set name

## Field Validation

### Overview
Field-level validation rules and constraints.

### CHECK Constraints

```sql
field1 CHECK (field1 IN ('A', 'I', 'D')),
field2 CHECK (field2 > 0),
field3 CHECK (LENGTH(field3) <= 50)
```

### Validation Expressions

```sql
field1 "CASE WHEN :field1 NOT IN ('A', 'B', 'C') THEN 'INVALID' ELSE :field1 END",
field2 "CASE WHEN LENGTH(:field2) < 3 THEN 'INVALID' ELSE :field2 END"
```

### Configuration
- **ValidationRule**: CHECK constraint expression
- **ValidationExpression**: Custom validation logic

## Advanced Date/Time Handling

### Overview
Complex date and time format specifications.

### Date Formats

```sql
field1 DATE "YYYY-MM-DD",
field2 DATE "MM/DD/YYYY",
field3 DATE "DD-MM-YYYY"
```

### Timestamp Formats

```sql
field1 TIMESTAMP "YYYY-MM-DD HH24:MI:SS",
field2 TIMESTAMP "YYYY-MM-DD HH24:MI:SS.FF3"
```

### Time Zone Formats

```sql
field1 TIMESTAMP "YYYY-MM-DD HH24:MI:SS TZH:TZM",
field2 TIMESTAMP WITH TIME ZONE "YYYY-MM-DD HH24:MI:SS TZH:TZM"
```

### Julian Dates

```sql
field1 DATE "YYYYDDD",
field2 DATE "YYDDD"
```

### Configuration
- **DateFormat**: Date format specification
- **TimeZoneFormat**: Time zone format specification

## Advanced Numeric Handling

### Overview
Complex numeric format specifications.

### External Formats

```sql
field1 NUMBER(10,2) EXTERNAL,
field2 FLOAT EXTERNAL,
field3 DECIMAL EXTERNAL
```

### Binary Formats

```sql
field1 BINARY_INTEGER,
field2 BINARY_FLOAT,
field3 BINARY_DOUBLE
```

### Packed Formats

```sql
field1 PACKED DECIMAL,
field2 ZONED DECIMAL
```

### Configuration
- **NumericFormat**: EXTERNAL, INTERNAL, PACKED, ZONED

## Usage Examples

### Example 1: Complex Employee Data

```sql
LOAD DATA
INFILE 'employees.csv'
BADFILE 'employees.bad'
DISCARDFILE 'employees.dsc'
APPEND
INTO TABLE employees
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    employee_id NUMBER(6) INTEGER EXTERNAL,
    first_name VARCHAR2(50) "UPPER(:first_name)" NULLIF first_name = BLANKS,
    last_name VARCHAR2(50) "UPPER(:last_name)" NULLIF last_name = BLANKS,
    email VARCHAR2(100) "LOWER(:email)" NULLIF email = BLANKS,
    hire_date DATE "YYYY-MM-DD" NULLIF hire_date = "0000-00-00",
    salary NUMBER(8,2) EXTERNAL ":salary * 100" NULLIF salary = "999999",
    status CHAR(1) "CASE WHEN :status = 'A' THEN 'ACTIVE' WHEN :status = 'I' THEN 'INACTIVE' ELSE 'UNKNOWN' END",
    department_id NUMBER(4) INTEGER EXTERNAL NULLIF department_id = "999",
    manager_id NUMBER(6) INTEGER EXTERNAL NULLIF manager_id = "999999",
    description VARCHAR2(200) OPTIONALLY ENCLOSED BY '"' CHARACTERSET UTF8,
    large_notes CLOB(1000) TERMINATED BY EOF,
    binary_data RAW(16) TERMINATED BY WHITESPACE,
    created_date TIMESTAMP "YYYY-MM-DD HH24:MI:SS" DEFAULTIF created_date = BLANKS "SYSDATE"
)
```

### Example 2: Fixed-Width Customer Data

```sql
LOAD DATA
INFILE 'customers.txt'
BADFILE 'customers.bad'
DISCARDFILE 'customers.dsc'
REPLACE
INTO TABLE customers
(
    customer_id POSITION(1:5) NUMBER(5) INTEGER EXTERNAL,
    customer_name POSITION(6:35) VARCHAR2(30) "UPPER(:customer_name)" NULLIF customer_name = BLANKS,
    address_line1 POSITION(36:65) VARCHAR2(30) NULLIF address_line1 = BLANKS,
    address_line2 POSITION(66:95) VARCHAR2(30) NULLIF address_line2 = BLANKS,
    city POSITION(96:115) VARCHAR2(20) "UPPER(:city)" NULLIF city = BLANKS,
    state POSITION(116:117) CHAR(2) "UPPER(:state)" NULLIF state = BLANKS,
    zip_code POSITION(118:127) VARCHAR2(10) NULLIF zip_code = BLANKS,
    phone POSITION(128:137) VARCHAR2(10) NULLIF phone = "0000000000",
    email POSITION(138:167) VARCHAR2(30) "LOWER(:email)" NULLIF email = BLANKS,
    registration_date POSITION(168:177) DATE "YYYY-MM-DD" NULLIF registration_date = "0000-00-00",
    status POSITION(178:178) CHAR(1) "CASE WHEN :status = 'A' THEN 'ACTIVE' WHEN :status = 'I' THEN 'INACTIVE' ELSE 'UNKNOWN' END",
    priority POSITION(179:188) VARCHAR2(10) DEFAULTIF priority = BLANKS "NORMAL",
    notes CLOB(500) POSITION(189:688) TERMINATED BY EOF
)
```

## Best Practices

### 1. Performance Considerations
- Use appropriate data types for your data
- Avoid unnecessary transformations
- Use EXTERNAL format for numeric data when possible
- Consider using DIRECT path loading for large datasets

### 2. Data Quality
- Always validate data before loading
- Use appropriate NULL conditions
- Implement field-level validation rules
- Handle edge cases with transformations

### 3. Character Sets
- Use UTF8 for international data
- Specify character sets explicitly
- Test with actual data samples

### 4. Error Handling
- Use appropriate bad and discard files
- Set reasonable error limits
- Monitor loading logs

### 5. Documentation
- Document complex transformations
- Maintain field specification templates
- Version control your control files

## Conclusion

The advanced field specifications provide powerful capabilities for handling complex data loading scenarios. By leveraging these features, you can create robust, efficient, and maintainable data loading processes that handle the complexities of real-world data.

For more information, refer to the Oracle SQL*Loader documentation and the example control files provided with this application. 