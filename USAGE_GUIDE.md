# Oracle SQL*Loader Control File Generator - Usage Guide

This guide provides detailed instructions for using the Oracle SQL*Loader Control File Generator application to create control files for loading data into Oracle databases.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Excel Import Mode](#excel-import-mode)
3. [Manual Entry Mode](#manual-entry-mode)
4. [Field Management](#field-management)
5. [Validation and Error Handling](#validation-and-error-handling)
6. [Settings Configuration](#settings-configuration)
7. [Export and Preview](#export-and-preview)
8. [Troubleshooting](#troubleshooting)

## Getting Started

### Launching the Application

1. **Start the Application**: Run `ControlFileGenerator.WinForms.exe`
2. **Initial Screen**: You'll see the main interface with:
   - Top panel: Excel import controls
   - Main area: DataGridView for field definitions
   - Bottom panel: Action buttons and status

### Understanding the Interface

- **Load Excel Metadata**: Import field definitions from Excel files
- **Start from Scratch**: Begin with an empty field list
- **Add Field**: Create new field definitions manually
- **Remove Field**: Delete selected fields
- **Toggle Mode**: Switch between fixed-width and CSV modes
- **Validate**: Check for errors and warnings
- **Auto Fix**: Automatically resolve common issues
- **Settings**: Configure global options
- **Preview**: View generated control file
- **Export .ctl**: Save the control file
- **Data Preview**: Preview actual data using field definitions

## Excel Import Mode

### Step 1: Prepare Your Excel File

Create an Excel file with the following columns (headers are flexible):

| Column | Required | Description | Example |
|--------|----------|-------------|---------|
| Field Name | Yes | Oracle column name | `EMPNO` |
| Order | No | Field order in file | `1` |
| Start Position | No | Starting position | `1` |
| End Position | No | Ending position | `6` |
| Length | No | Field length | `6` |
| COBOL Type | No | COBOL type definition | `PIC 9(6)` |
| SQL Type | No | Oracle SQL type | `NUMBER` |
| Nullable | No | YES/NO | `YES` |
| Transform | No | SQL transformation | `UPPER(:ENAME)` |
| Default Value | No | Default value | `USA` |
| Null If Value | No | NULL condition | `BLANKS` |
| Enclosed By | No | Quote character | `"` |
| Delimiter | No | Field delimiter | `,` |
| Data Format | No | Date/number format | `YYYYMMDD` |
| Description | No | Field description | `Employee Number` |

### Step 2: Import Excel File

1. **Click "Load Excel Metadata"**
2. **Select your Excel file** (.xlsx or .xls format)
3. **Choose worksheet** from the dropdown (if multiple sheets)
4. **Review imported fields** in the DataGridView

### Step 3: Review and Edit

- **Check field names**: Ensure they match your Oracle table
- **Verify data types**: Review auto-inferred SQL types
- **Adjust positions**: Modify start/end positions if needed
- **Add transformations**: Include data transformations as needed

## Manual Entry Mode

### Step 1: Start from Scratch

1. **Click "Start from Scratch"**
2. **Clear existing fields** (if any)
3. **Begin with empty field list**

### Step 2: Add Fields

1. **Click "Add Field"** for each field you need
2. **Fill in required information**:
   - **Field Name**: Oracle column name
   - **Order**: Sequential order (1, 2, 3, etc.)
   - **COBOL Type**: If available (e.g., `PIC 9(6)`)
   - **SQL Type**: Oracle data type (auto-inferred)

### Step 3: Configure Field Properties

For each field, set appropriate properties:

#### Basic Properties
- **Field Name**: Must be unique
- **Order**: Determines field sequence
- **COBOL Type**: Used for type inference
- **SQL Type**: Oracle data type

#### Position Properties (Fixed Width)
- **Start Position**: Beginning character position
- **End Position**: Ending character position
- **Length**: Automatically calculated

#### Advanced Properties
- **Nullable**: YES/NO
- **Transform**: SQL expression (e.g., `UPPER(:FIELD)`)
- **Default Value**: Default if NULL
- **Null If Value**: Value to treat as NULL
- **Data Format**: For dates/numbers

## Field Management

### Adding Fields

1. **Click "Add Field"**
2. **New field appears** at the bottom of the list
3. **Fill in required properties**
4. **Use "Auto Fix"** to infer missing information

### Removing Fields

1. **Select the field** in the DataGridView
2. **Click "Remove Field"**
3. **Confirm deletion** if prompted

### Editing Fields

1. **Click directly in the cell** you want to edit
2. **Type new value**
3. **Press Enter** or click elsewhere to save
4. **Validation occurs automatically**

### Reordering Fields

1. **Edit the Order column** for each field
2. **Use sequential numbers** (1, 2, 3, etc.)
3. **Click "Auto Fix"** to recalculate positions

## Validation and Error Handling

### Visual Indicators

The application uses color coding to show field status:

- **Green**: Valid field
- **Yellow**: Warning (non-critical issue)
- **Red**: Error (must be fixed)

### Running Validation

1. **Click "Validate"** to check all fields
2. **Review validation results** in status area
3. **Fix errors** before proceeding
4. **Use "Auto Fix"** for automatic corrections

### Common Validation Issues

#### Missing Required Fields
- **Field Name**: Must be provided
- **Order**: Must be sequential
- **SQL Type**: Will be auto-inferred if missing

#### Position Issues
- **Overlapping positions**: Fields overlap in fixed-width mode
- **Missing positions**: Start/end positions not provided
- **Gaps**: Unused positions between fields

#### Type Issues
- **Invalid SQL types**: Not recognized Oracle types
- **COBOL mapping**: Unable to map COBOL type to Oracle
- **Format issues**: Invalid date/number formats

### Auto Fix Features

The "Auto Fix" button can resolve:

- **Missing SQL types**: Infer from COBOL types
- **Missing positions**: Calculate based on order
- **Type mismatches**: Suggest appropriate types
- **Format issues**: Apply standard formats

## Settings Configuration

### Accessing Settings

1. **Click "Settings"** button
2. **Settings dialog opens** with multiple tabs

### Table Configuration

- **Table Name**: Target Oracle table
- **Load Mode**: APPEND, REPLACE, INSERT, TRUNCATE
- **Trailing Null Columns**: Handle trailing NULLs

### File Configuration

- **Input File**: Path to data file
- **Bad File**: Path for rejected records
- **Discard File**: Path for discarded records
- **Encoding**: Character encoding (UTF8, etc.)

### Processing Options

- **Max Errors**: Maximum errors before stopping
- **Bind Size**: Memory allocation for loading
- **Rows**: Rows per commit
- **Skip Rows**: Header rows to skip
- **Use Direct Path**: Enable direct path loading

### CSV/Delimited Options

- **Field Terminator**: Character separating fields
- **Enclosed By**: Quote character
- **Optionally Enclosed**: Whether quotes are optional
- **Trim Option**: How to handle whitespace

## Export and Preview

### Previewing Control File

1. **Click "Preview"** to see generated control file
2. **Review syntax** and field mappings
3. **Check for errors** in the preview
4. **Copy to clipboard** if needed

### Exporting Control File

1. **Click "Export .ctl"**
2. **Choose save location**
3. **Enter filename** (with .ctl extension)
4. **Click Save**

### Data Preview

1. **Configure data file path** in Settings
2. **Click "Data Preview"**
3. **View sample data** using field definitions
4. **Verify field mappings** are correct

### JSON Export

1. **Use Field Definition Exporter** service
2. **Save field configuration** as JSON
3. **Reuse configuration** in future sessions

## Mode Switching

### Fixed Width Mode

Use for files with fixed-length fields:

```
EMPNO     SMITH     SALARY    DATE
123456    JOHN      50000     20240101
```

**Features:**
- Position-based field definitions
- No delimiters between fields
- Precise character positioning

### CSV/Delimited Mode

Use for comma-separated or delimited files:

```
EMPNO,ENAME,SALARY,DATE
123456,"SMITH, JOHN",50000,2024-01-01
```

**Features:**
- Delimiter-separated fields
- Optional field enclosure
- Flexible field lengths

### Switching Modes

1. **Click "Toggle Mode"**
2. **Mode indicator changes** (Fixed Width ↔ CSV/Delimited)
3. **Field properties adjust** automatically
4. **Validation rules update** for new mode

## Troubleshooting

### Common Issues and Solutions

#### Excel Import Problems

**Issue**: Excel file not loading
**Solution**: 
- Check file format (.xlsx or .xls)
- Verify column headers exist
- Ensure file is not corrupted

**Issue**: Wrong worksheet selected
**Solution**:
- Use dropdown to select correct worksheet
- Check worksheet names in Excel

#### Validation Errors

**Issue**: Missing field names
**Solution**:
- Fill in all Field Name cells
- Ensure names are unique
- Use descriptive names

**Issue**: Position overlaps
**Solution**:
- Check Start/End positions
- Use "Auto Fix" to recalculate
- Adjust field lengths

**Issue**: Invalid SQL types
**Solution**:
- Check COBOL type format
- Use "Auto Fix" to infer types
- Manually specify Oracle types

#### Export Problems

**Issue**: Control file syntax errors
**Solution**:
- Review preview before export
- Check table name and field mappings
- Verify Oracle syntax

**Issue**: File not saving
**Solution**:
- Check file permissions
- Ensure directory exists
- Use valid filename

### Performance Tips

1. **Use Direct Path** for large files
2. **Adjust Bind Size** based on available memory
3. **Set appropriate Rows** per commit
4. **Use external tables** for very large datasets

### Best Practices

1. **Always validate** before export
2. **Use descriptive field names**
3. **Include field descriptions** for documentation
4. **Test with sample data** before production
5. **Backup configurations** as JSON files

## Advanced Features

### Custom Transformations

Use SQL expressions for data transformation:

- **Case conversion**: `UPPER(:FIELD)`, `LOWER(:FIELD)`
- **String manipulation**: `TRIM(:FIELD)`, `SUBSTR(:FIELD, 1, 10)`
- **Mathematical operations**: `:FIELD * 100`, `:FIELD + 1`
- **Date formatting**: `TO_DATE(:FIELD, 'YYYYMMDD')`

### Conditional Loading

Use NULLIF clauses for conditional loading:

- **Null if blank**: `NULLIF FIELD = BLANKS`
- **Null if specific value**: `NULLIF FIELD = '99999'`
- **Null if zero**: `NULLIF FIELD = 0`

### Complex Field Types

Handle various data types:

- **Dates**: Use appropriate format strings
- **Numbers**: Specify precision and scale
- **Characters**: Choose CHAR vs VARCHAR2
- **Large objects**: Use CLOB/BLOB for large data

This comprehensive guide should help you effectively use the Oracle SQL*Loader Control File Generator application for all your data loading needs. 