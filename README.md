# Oracle Loader Control Files (CTL Files)

Oracle Loader Control Files are configuration files used with SQL*Loader utility to define how data should be loaded into Oracle database tables. They have a `.ctl` extension and contain directives that tell SQL*Loader how to process input data files.

## Key Components

### 1. Basic Structure
- **LOAD DATA** - Specifies the type of data loading operation
- **INFILE** - Defines the input data file location
- **INTO TABLE** - Specifies the target table
- **FIELDS TERMINATED BY** - Defines field delimiters
- **TRAILING NULLCOLS** - Handles missing columns
- **Column specifications** - Maps data file columns to table columns

### 2. Loading Modes
- **APPEND** - Add new records to existing table
- **REPLACE** - Delete existing data and load new data
- **INSERT** - Load only if table is empty
- **TRUNCATE** - Truncate table before loading

### 3. File Specifications
- **BADFILE** - Records that fail validation
- **DISCARDFILE** - Records that don't meet WHEN conditions
- **LOGFILE** - SQL*Loader execution log

## Examples Included

### 1. Basic Loader (`example_loader.ctl`)
Simple CSV file loading with basic field mapping and date formatting.

### 2. Advanced Loader (`advanced_loader.ctl`)
Demonstrates:
- Conditional loading with WHEN clauses
- Multiple input files
- Different data types (INTEGER, DECIMAL, CHAR, DATE)
- External data type specifications

### 3. Fixed-Width Loader (`fixed_width_loader.ctl`)
Shows how to handle fixed-width format files using POSITION specifications.

## Common Directives

### Data Type Specifications
- `INTEGER EXTERNAL` - Numeric data
- `DECIMAL EXTERNAL` - Decimal numbers
- `CHAR(n)` - Character data with length
- `DATE "format"` - Date data with format specification

### Field Delimiters
- `FIELDS TERMINATED BY ','` - Comma-separated
- `FIELDS TERMINATED BY '|'` - Pipe-separated
- `FIELDS TERMINATED BY '\t'` - Tab-separated

### Conditional Loading
- `WHEN (position) = 'value'` - Load only records matching condition
- `WHEN (1:3) = 'EMP'` - Load records starting with 'EMP'

## Usage

To run SQL*Loader with a control file:

```bash
sqlldr username/password@database control=loader.ctl
```

Or with additional parameters:

```bash
sqlldr username/password@database control=loader.ctl log=loader.log bad=loader.bad
```

## Best Practices

1. **Always specify BADFILE and DISCARDFILE** for error handling
2. **Use appropriate data types** for each column
3. **Handle NULL values** with TRAILING NULLCOLS
4. **Test with small datasets** before loading large files
5. **Review log files** for any issues
6. **Use descriptive names** for control files and log files

## Error Handling

- **BADFILE** contains records that failed to load due to data type mismatches or constraint violations
- **DISCARDFILE** contains records that didn't meet WHEN conditions
- **LOGFILE** contains detailed information about the loading process

## Performance Tips

1. Use **DIRECT=TRUE** for faster loading (bypasses buffer cache)
2. Set appropriate **ROWS** parameter for batch processing
3. Use **PARALLEL** loading for large datasets
4. Consider **SKIP** parameter to skip header rows
5. Use **CONSTANT** for default values 