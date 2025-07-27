# Oracle SQL*Loader Data Types Complete Reference

## Table of Contents
1. [Numeric Data Types](#numeric-data-types)
2. [Character Data Types](#character-data-types)
3. [Date and Time Data Types](#date-and-time-data-types)
4. [Large Object (LOB) Data Types](#large-object-lob-data-types)
5. [Raw Data Types](#raw-data-types)
6. [Special Data Types](#special-data-types)
7. [Data Type Modifiers](#data-type-modifiers)
8. [Format Specifications](#format-specifications)
9. [Examples by Use Case](#examples-by-use-case)

## Numeric Data Types

### INTEGER
```sql
-- Basic integer
INTEGER EXTERNAL

-- With precision
INTEGER EXTERNAL(5)

-- Examples
employee_id INTEGER EXTERNAL,
quantity INTEGER EXTERNAL(6),
status_code INTEGER EXTERNAL(3)
```

### DECIMAL
```sql
-- Basic decimal
DECIMAL EXTERNAL

-- With precision and scale
DECIMAL EXTERNAL(10,2)

-- Examples
salary DECIMAL EXTERNAL(8,2),
price DECIMAL EXTERNAL(10,4),
percentage DECIMAL EXTERNAL(5,2)
```

### FLOAT
```sql
-- Basic float
FLOAT EXTERNAL

-- With precision
FLOAT EXTERNAL(10)

-- Examples
scientific_value FLOAT EXTERNAL,
measurement FLOAT EXTERNAL(15)
```

### DOUBLE
```sql
-- Double precision
DOUBLE EXTERNAL

-- Examples
high_precision_value DOUBLE EXTERNAL
```

### SMALLINT
```sql
-- Small integer
SMALLINT EXTERNAL

-- Examples
age SMALLINT EXTERNAL,
rating SMALLINT EXTERNAL
```

## Character Data Types

### CHAR
```sql
-- Fixed length character
CHAR(50)
CHAR(100) LDRTRIM

-- Examples
first_name CHAR(30),
last_name CHAR(50) LDRTRIM,
code CHAR(10)
```

### VARCHAR2
```sql
-- Variable length character
VARCHAR2(100)
VARCHAR2(500) LDRTRIM

-- Examples
description VARCHAR2(200),
notes VARCHAR2(1000),
address VARCHAR2(150)
```

### VARCHAR
```sql
-- Variable length character (synonym for VARCHAR2)
VARCHAR(200)

-- Examples
title VARCHAR(100),
comments VARCHAR(500)
```

### NCHAR
```sql
-- National character set fixed length
NCHAR(50)

-- Examples
multilingual_name NCHAR(100)
```

### NVARCHAR2
```sql
-- National character set variable length
NVARCHAR2(100)

-- Examples
multilingual_text NVARCHAR2(500)
```

## Date and Time Data Types

### DATE
```sql
-- Date with format specification
DATE "YYYY-MM-DD"
DATE "MM/DD/YYYY"
DATE "DD-MON-YYYY"
DATE "YYYY-MM-DD HH24:MI:SS"

-- Examples
hire_date DATE "YYYY-MM-DD",
birth_date DATE "MM/DD/YYYY",
created_date DATE "DD-MON-YYYY",
timestamp DATE "YYYY-MM-DD HH24:MI:SS"
```

### TIMESTAMP
```sql
-- Timestamp with fractional seconds
TIMESTAMP "YYYY-MM-DD HH24:MI:SS.FF"
TIMESTAMP "MM/DD/YYYY HH:MI:SS AM"

-- Examples
created_timestamp TIMESTAMP "YYYY-MM-DD HH24:MI:SS.FF6",
updated_timestamp TIMESTAMP "MM/DD/YYYY HH:MI:SS AM"
```

### TIMESTAMP WITH TIME ZONE
```sql
-- Timestamp with time zone
TIMESTAMP WITH TIME ZONE "YYYY-MM-DD HH24:MI:SS.FF TZH:TZM"

-- Examples
global_timestamp TIMESTAMP WITH TIME ZONE "YYYY-MM-DD HH24:MI:SS.FF TZH:TZM"
```

### TIMESTAMP WITH LOCAL TIME ZONE
```sql
-- Timestamp with local time zone
TIMESTAMP WITH LOCAL TIME ZONE "YYYY-MM-DD HH24:MI:SS.FF"

-- Examples
local_timestamp TIMESTAMP WITH LOCAL TIME ZONE "YYYY-MM-DD HH24:MI:SS.FF"
```

### INTERVAL YEAR TO MONTH
```sql
-- Interval for years and months
INTERVAL YEAR TO MONTH

-- Examples
duration INTERVAL YEAR TO MONTH
```

### INTERVAL DAY TO SECOND
```sql
-- Interval for days, hours, minutes, seconds
INTERVAL DAY TO SECOND

-- Examples
time_duration INTERVAL DAY TO SECOND
```

## Large Object (LOB) Data Types

### CLOB
```sql
-- Character large object
CLOB
CLOB(1000000)

-- With file reference
CLOB(1000000) LOBFILE(filename) TERMINATED BY EOF
CLOB(1000000) LOBFILE(filename) TERMINATED BY '|'

-- Examples
document_content CLOB(5000000),
description CLOB(1000000) LOBFILE(doc_id) TERMINATED BY EOF
```

### BLOB
```sql
-- Binary large object
BLOB
BLOB(1000000)

-- With file reference
BLOB(1000000) LOBFILE(filename) TERMINATED BY EOF

-- Examples
image_data BLOB(10000000),
file_content BLOB(5000000) LOBFILE(file_id) TERMINATED BY EOF
```

### NCLOB
```sql
-- National character large object
NCLOB
NCLOB(1000000)

-- Examples
multilingual_content NCLOB(2000000)
```

## Raw Data Types

### RAW
```sql
-- Raw binary data
RAW(1000)
RAW(5000)

-- Examples
binary_data RAW(2000),
hash_value RAW(64)
```

### LONG RAW
```sql
-- Long raw binary data
LONG RAW

-- Examples
large_binary LONG RAW
```

## Special Data Types

### XMLTYPE
```sql
-- XML data type
XMLTYPE

-- Examples
xml_content XMLTYPE,
configuration XMLTYPE
```

### JSON
```sql
-- JSON data type
JSON

-- Examples
metadata JSON,
settings JSON
```

### BFILE
```sql
-- Binary file reference
BFILE

-- Examples
external_file BFILE
```

## Data Type Modifiers

### EXTERNAL
```sql
-- External data type (default for most types)
INTEGER EXTERNAL
DECIMAL EXTERNAL(10,2)
CHAR(50) EXTERNAL
```

### INTERNAL
```sql
-- Internal data type (Oracle internal format)
INTEGER INTERNAL
DECIMAL INTERNAL
```

### LDRTRIM
```sql
-- Trim options for character data
CHAR(50) LDRTRIM          -- Left and right trim
CHAR(50) LTRIM            -- Left trim only
CHAR(50) RTRIM            -- Right trim only
CHAR(50) NOTRIM           -- No trimming
```

### VARRAW
```sql
-- Variable length raw data
CHAR(1000) VARRAW

-- Examples
variable_binary CHAR(5000) VARRAW
```

## Format Specifications

### Date Formats
```sql
-- Common date formats
"YYYY-MM-DD"              -- 2023-12-25
"MM/DD/YYYY"              -- 12/25/2023
"DD-MON-YYYY"             -- 25-DEC-2023
"DD-MON-YY"               -- 25-DEC-23
"YYYY-MM-DD HH24:MI:SS"   -- 2023-12-25 14:30:45
"MM/DD/YYYY HH:MI:SS AM"  -- 12/25/2023 02:30:45 PM
```

### Timestamp Formats
```sql
-- Timestamp formats with fractional seconds
"YYYY-MM-DD HH24:MI:SS.FF"        -- 2023-12-25 14:30:45.123456
"MM/DD/YYYY HH:MI:SS.FF AM"       -- 12/25/2023 02:30:45.123 PM
"YYYY-MM-DD HH24:MI:SS.FF TZH:TZM" -- 2023-12-25 14:30:45.123 -05:00
```

### Numeric Formats
```sql
-- Numeric format specifications
DECIMAL EXTERNAL(10,2)    -- 1234567.89
INTEGER EXTERNAL(5)       -- 12345
FLOAT EXTERNAL(15)        -- 1.23456789012345E+10
```

## Examples by Use Case

### Employee Data Loading
```sql
(
    employee_id INTEGER EXTERNAL(6),
    first_name CHAR(30) LDRTRIM,
    last_name CHAR(50) LDRTRIM,
    email VARCHAR2(100),
    hire_date DATE "YYYY-MM-DD",
    salary DECIMAL EXTERNAL(8,2),
    department_id INTEGER EXTERNAL(4),
    manager_id INTEGER EXTERNAL(6),
    photo BLOB(1000000) LOBFILE(employee_id) TERMINATED BY EOF
)
```

### Financial Data Loading
```sql
(
    transaction_id INTEGER EXTERNAL(10),
    account_number CHAR(20),
    transaction_date DATE "YYYY-MM-DD HH24:MI:SS",
    amount DECIMAL EXTERNAL(15,2),
    currency CHAR(3),
    description VARCHAR2(200),
    reference_number CHAR(50)
)
```

### Document Management
```sql
(
    document_id INTEGER EXTERNAL(8),
    document_name VARCHAR2(200),
    document_type CHAR(10),
    file_size INTEGER EXTERNAL(10),
    upload_date TIMESTAMP "YYYY-MM-DD HH24:MI:SS.FF",
    content CLOB(10000000) LOBFILE(document_id) TERMINATED BY EOF,
    metadata JSON,
    checksum RAW(64)
)
```

### Scientific Data Loading
```sql
(
    sample_id INTEGER EXTERNAL(8),
    measurement_date DATE "YYYY-MM-DD",
    temperature FLOAT EXTERNAL(10),
    pressure FLOAT EXTERNAL(10),
    humidity FLOAT EXTERNAL(5),
    location VARCHAR2(100),
    notes CLOB(5000)
)
```

### E-commerce Data Loading
```sql
(
    order_id INTEGER EXTERNAL(10),
    customer_id INTEGER EXTERNAL(8),
    order_date DATE "YYYY-MM-DD HH24:MI:SS",
    total_amount DECIMAL EXTERNAL(10,2),
    shipping_address VARCHAR2(300),
    billing_address VARCHAR2(300),
    payment_method CHAR(20),
    order_status CHAR(20),
    tracking_number VARCHAR2(50)
)
```

## Best Practices

### 1. Data Type Selection
- Use appropriate data types for your data
- Consider storage requirements
- Plan for future data growth

### 2. Performance Considerations
- Use fixed-length types when possible
- Consider compression for large objects
- Optimize for your specific use case

### 3. Data Validation
- Validate data before loading
- Use appropriate constraints
- Handle NULL values properly

### 4. Character Set Handling
- Specify character sets when needed
- Consider multilingual requirements
- Use appropriate national character types

### 5. Large Object Handling
- Use LOBFILE for external file references
- Consider chunking for very large objects
- Monitor storage requirements

This reference provides comprehensive coverage of all Oracle data types available in SQL*Loader control files, enabling you to build robust data loading solutions. 