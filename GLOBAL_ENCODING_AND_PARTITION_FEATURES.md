# Global File Encoding and Partition Features

## Overview

This document describes the new features added to the Oracle SQL*Loader Control File Generator:

1. **Global File Encoding Support** - Allows setting CHARACTERSET for the entire data file
2. **Partition-Aware Options** - Supports loading data into specific table partitions

## Global File Encoding Feature

### Description
The global file encoding feature allows you to specify the character set for the entire data file being loaded. This is different from the field-level character set and applies to the LOAD DATA statement.

### Usage
```sql
LOAD DATA CHARACTERSET UTF8
```

### Supported Character Sets
The following character sets are available in the dropdown:

- **UTF8** - Unicode UTF-8 encoding (default)
- **WE8ISO8859P1** - Western European ISO 8859-1
- **AL32UTF8** - Unicode UTF-8 with AL32UTF8 encoding
- **JA16SJIS** - Japanese Shift-JIS encoding
- **ZHS16GBK** - Simplified Chinese GBK encoding
- **KO16MSWIN949** - Korean MS Windows 949 encoding

### UI Implementation
- **Location**: File References tab in Settings
- **Control**: Dropdown combo box labeled "Global File Encoding"
- **Default**: UTF8
- **Behavior**: Only generates CHARACTERSET clause if different from UTF8

## Partition-Aware Options

### Description
The partition-aware feature allows you to specify a target partition for data loading, bypassing Oracle's automatic partition routing.

### Usage
```sql
INTO TABLE EMPLOYEES
PARTITION (EMPLOYEES_2024)
APPEND
```

### UI Implementation

#### A) Optional Partition Name Entry
- **Location**: File References tab in Settings
- **Control**: Text box labeled "Partition Name"
- **Tooltip**: "If set, all data will be loaded into this specific partition"
- **Behavior**: Only enabled when partition loading is checked

#### B) Partition Loading Switch
- **Location**: File References tab in Settings
- **Control**: Checkbox labeled "Load into specific partition (advanced)"
- **Behavior**: 
  - When checked: Shows and enables Partition Name field
  - When unchecked: Hides Partition Name field, uses Oracle's automatic routing

### Advanced Features
- **Dynamic UI**: Partition name field is automatically enabled/disabled based on checkbox state
- **Validation**: Partition name is validated for proper Oracle identifier format
- **Default Behavior**: When unchecked, Oracle automatically routes data based on partition key

## Implementation Details

### Model Changes (LoaderConfig.cs)
```csharp
[DisplayName("Global File Encoding")]
public string GlobalFileEncoding { get; set; } = "UTF8";

[DisplayName("Use Specific Partition")]
public bool UseSpecificPartition { get; set; } = false;

[DisplayName("Partition Name")]
public string PartitionName { get; set; } = string.Empty;
```

### New Methods
```csharp
public string GetGlobalFileEncodingString()
{
    if (string.IsNullOrEmpty(GlobalFileEncoding) || 
        GlobalFileEncoding.Equals("UTF8", StringComparison.OrdinalIgnoreCase))
    {
        return string.Empty;
    }
    return $"CHARACTERSET {GlobalFileEncoding}";
}

public string GetPartitionString()
{
    if (!UseSpecificPartition || string.IsNullOrWhiteSpace(PartitionName))
    {
        return string.Empty;
    }
    return $"PARTITION ({PartitionName})";
}
```

### Control File Generation Changes
The `ControlFileGenerator` service now:
1. Uses `GetGlobalFileEncodingString()` instead of `GetCharacterSetString()` for the LOAD DATA clause
2. Adds partition specification after INTO TABLE if partition loading is enabled

## Example Generated Control File

```sql
-- Oracle SQL*Loader Control File
-- Generated on: 2024-01-15 10:30:00
-- Table: EMPLOYEES

LOAD DATA CHARACTERSET AL32UTF8
INFILE 'employees_data.csv'
BADFILE 'employees.bad'
DISCARDFILE 'employees.dsc'
SKIP 1
ERRORS 50
ROWS 50000
BINDSIZE 1048576
DIRECT=TRUE

INTO TABLE EMPLOYEES
PARTITION (EMPLOYEES_2024)
APPEND
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' LRTRIM
TRAILING NULLCOLS
(
    EMPLOYEE_ID INTEGER EXTERNAL,
    FIRST_NAME CHAR(20),
    LAST_NAME CHAR(25),
    EMAIL CHAR(25)
)
```

## Benefits

### Global File Encoding
- **Internationalization**: Support for various character encodings
- **Data Integrity**: Ensures proper character handling for multi-language data
- **Flexibility**: Choose the most appropriate encoding for your data source

### Partition-Aware Loading
- **Performance**: Direct loading into specific partitions
- **Control**: Bypass automatic partition routing when needed
- **Flexibility**: Support for both automatic and manual partition assignment
- **Advanced Usage**: Useful for maintenance operations and data archiving

## Best Practices

### Global File Encoding
1. **Match Source Encoding**: Choose the encoding that matches your source data file
2. **UTF8 Default**: Use UTF8 unless you have specific requirements
3. **Test Thoroughly**: Verify character handling with sample data

### Partition Loading
1. **Automatic Routing**: Use automatic routing (unchecked) for normal operations
2. **Specific Partitions**: Use specific partition loading for maintenance tasks
3. **Validation**: Ensure partition names follow Oracle naming conventions
4. **Performance**: Consider the impact on parallel loading operations

## Migration Notes

- **Backward Compatibility**: Existing configurations continue to work unchanged
- **Default Values**: New features default to safe, non-intrusive values
- **UI Layout**: New controls are added to the File References tab for logical grouping 