# Oracle SQL*Loader Control File Complete Reference

## Table of Contents
1. [Basic Syntax Structure](#basic-syntax-structure)
2. [Loading Modes](#loading-modes)
3. [File Specifications](#file-specifications)
4. [Field Specifications](#field-specifications)
5. [Data Type Specifications](#data-type-specifications)
6. [Conditional Loading](#conditional-loading)
7. [Performance Options](#performance-options)
8. [Error Handling](#error-handling)
9. [Advanced Features](#advanced-features)
10. [Command Line Parameters](#command-line-parameters)
11. [Complete Examples](#complete-examples)

## Basic Syntax Structure

### Standard Control File Format
```sql
LOAD DATA
[CHARACTERSET charset]
[BYTEORDERMARK (YES | NO)]
[BYTEORDER (LITTLE | BIG)]
[STRING SIZES ARE IN (BYTES | CHARACTERS)]
[LENGTH SEMANTICS (BYTE | CHAR)]
[PREPROCESSOR executable]
[INFILE file_specification]
[BADFILE file_specification]
[DISCARDFILE file_specification]
[LOGFILE file_specification]
[SKIP n]
[LOAD n]
[ERRORS n]
[ROWS n]
[BINDSIZE n]
[READSIZE n]
[DIRECT (TRUE | FALSE)]
[PARALLEL (TRUE | FALSE)]
[SKIP_INDEX_MAINTENANCE (TRUE | FALSE)]
[SKIP_UNUSABLE_INDEXES (TRUE | FALSE)]
[RESUMABLE (TRUE | FALSE)]
[RESUMABLE_NAME name]
[RESUMABLE_TIMEOUT n]
[EXTERNAL_TABLE (NOT_USED | GENERATE_ONLY | EXECUTE)]
[COLUMNARRAYROWS n]
[MULTITHREADING (TRUE | FALSE)]
[STREAMSIZE n]
[INTO TABLE table_name]
[APPEND | REPLACE | INSERT | TRUNCATE]
[WHEN condition]
[FIELDS [TERMINATED BY string] [OPTIONALLY] ENCLOSED BY char]
[LDRTRIM (LRTRIM | LTRIM | RTRIM | NOTRIM)]
[PRESERVE BLANKS]
[field_specifications]
```

## Loading Modes

### 1. APPEND
```sql
INTO TABLE employees APPEND
```
- Adds new records to existing table
- Requires table to exist
- Preserves existing data

### 2. REPLACE
```sql
INTO TABLE employees REPLACE
```
- Deletes all existing data
- Loads new data
- Equivalent to DELETE + INSERT

### 3. INSERT
```sql
INTO TABLE employees INSERT
```
- Loads data only if table is empty
- Fails if table contains data

### 4. TRUNCATE
```sql
INTO TABLE employees TRUNCATE
```
- Truncates table before loading
- Faster than REPLACE for large tables
- Requires TRUNCATE privilege

## File Specifications

### INFILE
```sql
-- Single file
INFILE 'data.csv'

-- Multiple files
INFILE 'data1.csv'
INFILE 'data2.csv'

-- Wildcard pattern
INFILE 'data*.csv'

-- Directory object
INFILE 'DATA_DIR:data.csv'

-- Stream (for real-time loading)
INFILE *
```

### BADFILE
```sql
BADFILE 'data.bad'
```
- Records that fail validation
- Data type mismatches
- Constraint violations

### DISCARDFILE
```sql
DISCARDFILE 'data.dsc'
```
- Records that don't meet WHEN conditions
- Records exceeding ERROR limit

### LOGFILE
```sql
LOGFILE 'loader.log'
```
- Detailed execution information
- Statistics and error messages

## Field Specifications

### Delimited Fields
```sql
-- Comma-separated
FIELDS TERMINATED BY ','

-- Tab-separated
FIELDS TERMINATED BY '\t'

-- Pipe-separated
FIELDS TERMINATED BY '|'

-- Multiple delimiters
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
```

### Fixed-Width Fields
```sql
-- Using POSITION
(
    id POSITION(1:5) INTEGER EXTERNAL,
    name POSITION(6:35) CHAR(30),
    date POSITION(36:45) DATE "YYYY-MM-DD"
)
```

### Variable-Length Fields
```sql
-- Length specified in first few characters
(
    id INTEGER EXTERNAL,
    name CHAR(30),
    data CHAR(1000) VARRAW
)
```

## Data Type Specifications

### Numeric Types
```sql
-- Integer
INTEGER EXTERNAL
INTEGER EXTERNAL(5)

-- Decimal
DECIMAL EXTERNAL
DECIMAL EXTERNAL(10,2)

-- Float
FLOAT EXTERNAL
FLOAT EXTERNAL(10)

-- Double
DOUBLE EXTERNAL
```

### Character Types
```sql
-- Fixed length
CHAR(50)
CHAR(100) LDRTRIM

-- Variable length
VARCHAR2(100)
VARCHAR(200)

-- Raw data
RAW(1000)
LONG RAW
```

### Date/Time Types
```sql
-- Date with format
DATE "YYYY-MM-DD"
DATE "MM/DD/YYYY"
DATE "DD-MON-YYYY"

-- Timestamp
TIMESTAMP "YYYY-MM-DD HH24:MI:SS.FF"
TIMESTAMP "MM/DD/YYYY HH:MI:SS AM"

-- Interval
INTERVAL YEAR TO MONTH
INTERVAL DAY TO SECOND
```

### Special Types
```sql
-- BLOB/CLOB
BLOB
CLOB

-- XML
XMLTYPE

-- JSON
JSON

-- LOB with file reference
CLOB(1000) LOBFILE(filename) TERMINATED BY EOF
```

## Conditional Loading

### WHEN Clause
```sql
-- Simple condition
WHEN (1:3) = 'EMP'

-- Multiple conditions
WHEN (1:3) = 'EMP' AND (10:12) = 'ACT'

-- Position-based conditions
WHEN employee_id > 1000

-- Field-based conditions
WHEN department_id = 10
```

### Complex Conditions
```sql
-- Using OR
WHEN (1:3) = 'EMP' OR (1:3) = 'MGR'

-- Using NOT
WHEN NOT (status = 'INACTIVE')

-- Using IN
WHEN department_id IN (10, 20, 30)
```

## Performance Options

### Direct Path Loading
```sql
DIRECT=TRUE
```
- Bypasses buffer cache
- Faster for large datasets
- Requires specific privileges

### Parallel Loading
```sql
PARALLEL=TRUE
```
- Uses multiple processes
- Requires partitioned tables
- Improves performance

### Batch Processing
```sql
ROWS=1000
BINDSIZE=8192
READSIZE=8192
```
- Controls batch size
- Memory usage optimization

### Stream Processing
```sql
STREAMSIZE=8192
COLUMNARRAYROWS=1000
```
- Real-time data processing
- Memory-efficient for large files

## Error Handling

### Error Limits
```sql
ERRORS=50
```
- Maximum number of errors before aborting
- Default is 50

### Skip Options
```sql
SKIP=1
```
- Skip header rows
- Skip corrupted records

### Resume Capability
```sql
RESUMABLE=TRUE
RESUMABLE_NAME='my_load'
RESUMABLE_TIMEOUT=3600
```
- Resume interrupted loads
- Automatic recovery

## Advanced Features

### Character Set Handling
```sql
CHARACTERSET UTF8
CHARACTERSET AL32UTF8
CHARACTERSET WE8ISO8859P1
```

### Byte Order Mark
```sql
BYTEORDERMARK YES
BYTEORDERMARK NO
```

### Preprocessor
```sql
PREPROCESSOR 'gzip -dc'
PREPROCESSOR 'unzip -p'
```

### External Tables
```sql
EXTERNAL_TABLE GENERATE_ONLY
EXTERNAL_TABLE EXECUTE
```

### Multithreading
```sql
MULTITHREADING=TRUE
```

## Command Line Parameters

### Basic Parameters
```bash
sqlldr username/password@database control=file.ctl
```

### Advanced Parameters
```bash
sqlldr username/password@database \
  control=file.ctl \
  log=file.log \
  bad=file.bad \
  discard=file.dsc \
  data=file.dat \
  direct=true \
  parallel=true \
  rows=1000 \
  errors=50 \
  skip=1 \
  load=10000
```

### Parameter Reference
| Parameter | Description | Default |
|-----------|-------------|---------|
| control | Control file name | Required |
| log | Log file name | filename.log |
| bad | Bad file name | filename.bad |
| discard | Discard file name | filename.dsc |
| data | Data file name | From control file |
| direct | Direct path loading | FALSE |
| parallel | Parallel loading | FALSE |
| rows | Rows per commit | 64 |
| errors | Max errors | 50 |
| skip | Skip rows | 0 |
| load | Max rows to load | All |
| bindsize | Bind array size | 256000 |
| readsize | Read buffer size | 1048576 |
| streamsize | Stream buffer size | 256000 |
| columnarrayrows | Column array rows | 5000 |
| resumable | Enable resumable | FALSE |
| resumable_name | Resumable name | System generated |
| resumable_timeout | Timeout seconds | 7200 |

## Complete Examples

### Example 1: Basic CSV Loading
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
    employee_id INTEGER EXTERNAL,
    first_name CHAR(50),
    last_name CHAR(50),
    email CHAR(100),
    hire_date DATE "YYYY-MM-DD",
    salary DECIMAL EXTERNAL,
    department_id INTEGER EXTERNAL
)
```

### Example 2: Fixed-Width with Multiple Tables
```sql
LOAD DATA
INFILE 'data.txt'
BADFILE 'data.bad'
DISCARDFILE 'data.dsc'
REPLACE
INTO TABLE employees
WHEN (1:3) = 'EMP'
(
    employee_id POSITION(1:5) INTEGER EXTERNAL,
    first_name POSITION(6:35) CHAR(30),
    last_name POSITION(36:65) CHAR(30),
    salary POSITION(66:75) DECIMAL EXTERNAL,
    hire_date POSITION(76:85) DATE "YYYY-MM-DD"
)
INTO TABLE managers
WHEN (1:3) = 'MGR'
(
    manager_id POSITION(1:5) INTEGER EXTERNAL,
    first_name POSITION(6:35) CHAR(30),
    last_name POSITION(36:65) CHAR(30),
    department_id POSITION(66:69) INTEGER EXTERNAL
)
```

### Example 3: Advanced with LOBs and Conditions
```sql
LOAD DATA
CHARACTERSET UTF8
INFILE 'complex_data.csv'
BADFILE 'complex.bad'
DISCARDFILE 'complex.dsc'
LOGFILE 'complex.log'
APPEND
INTO TABLE documents
WHEN document_type IN ('PDF', 'DOC', 'TXT')
FIELDS TERMINATED BY '|' OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    doc_id INTEGER EXTERNAL,
    doc_name CHAR(100),
    doc_type CHAR(10),
    file_size INTEGER EXTERNAL,
    upload_date DATE "YYYY-MM-DD HH24:MI:SS",
    content CLOB(1000000) LOBFILE(doc_id) TERMINATED BY EOF,
    metadata JSON,
    status CHAR(1) DEFAULT 'A'
)
```

### Example 4: Performance Optimized Loading
```sql
LOAD DATA
INFILE 'large_data.csv'
BADFILE 'large_data.bad'
DISCARDFILE 'large_data.dsc'
LOGFILE 'large_data.log'
APPEND
INTO TABLE large_table
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    id INTEGER EXTERNAL,
    name CHAR(100),
    description VARCHAR2(500),
    created_date DATE "YYYY-MM-DD HH24:MI:SS",
    data_value DECIMAL EXTERNAL(15,2)
)
```

## Best Practices

### 1. Error Handling
- Always specify BADFILE and DISCARDFILE
- Set appropriate ERROR limits
- Review log files after loading

### 2. Performance
- Use DIRECT=TRUE for large datasets
- Set appropriate ROWS and BINDSIZE values
- Consider PARALLEL loading for partitioned tables

### 3. Data Validation
- Use appropriate data types
- Handle NULL values properly
- Validate data before loading

### 4. Security
- Use directory objects for file access
- Limit file permissions
- Validate input data

### 5. Monitoring
- Monitor log files
- Track loading statistics
- Set up alerts for failures

## Troubleshooting

### Common Issues
1. **Permission Denied**: Check file and directory permissions
2. **Character Set Issues**: Verify CHARACTERSET setting
3. **Memory Issues**: Adjust BINDSIZE and ROWS parameters
4. **Performance Issues**: Enable DIRECT loading and optimize parameters
5. **Data Type Mismatches**: Review field specifications

### Debugging Tips
1. Start with small datasets
2. Use detailed logging
3. Check BADFILE for rejected records
4. Verify data file format
5. Test with sample data first

This reference document provides comprehensive coverage of all Oracle SQL*Loader Control File options and can be used as a foundation for building automated tools and utilities. 