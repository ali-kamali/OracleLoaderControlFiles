# Oracle SQL*Loader Control File Generator - Usage Guide

## Overview

This guide explains how to use the enhanced Oracle SQL*Loader Control File Generator with comprehensive COBOL to Oracle type mapping, edge case handling, and validation features.

## Getting Started

### Prerequisites
- .NET 6.0 or later
- Windows Forms application
- Excel files (optional) for importing field definitions

### Installation
1. Build the solution using Visual Studio or `dotnet build`
2. Run the application: `dotnet run` or execute the compiled executable

## Core Features

### 1. COBOL to Oracle Type Mapping

The application automatically maps COBOL data types to Oracle SQL types:

#### Automatic Type Inference
- **COBOL Type:** `PIC 9(10)` → **Oracle Type:** `NUMBER(10)`
- **COBOL Type:** `PIC X(50)` → **Oracle Type:** `VARCHAR2(50)`
- **COBOL Type:** `PIC 9(8)V99` → **Oracle Type:** `NUMBER(10,2)`

#### Manual Override
You can manually specify Oracle types if the automatic mapping doesn't meet your needs.

### 2. Edge Case Handling

The application handles various edge cases gracefully:

#### Missing SQL Types
- Automatically infers Oracle types from COBOL types
- Defaults to `VARCHAR2(255)` if no COBOL type is available

#### Missing Positions
- Calculates start/end positions based on field order and length
- Uses sequential positioning when order is available

#### Overlapping Positions
- Detects and warns about overlapping field positions
- Allows manual correction without crashing

#### Duplicate Field Names
- Warns about duplicate field names (case-insensitive)
- Allows editing to resolve conflicts

### 3. Validation System

#### Real-time Validation
- Fields are validated as you edit them
- Color-coded highlighting:
  - **Green:** Valid fields
  - **Yellow:** Fields with warnings
  - **Red:** Fields with errors

#### Validation Rules
- Field names are required and non-empty
- SQL types must be valid Oracle data types
- Length values must be positive
- Start positions must be ≤ end positions

### 4. Starting from Scratch

You can create field definitions without importing from Excel:

1. **Generate Default Fields:** Creates 5 default fields with basic properties
2. **Add Fields:** Add new fields with auto-incremented order
3. **Remove Fields:** Delete selected fields and reorder remaining
4. **Edit Fields:** Modify field properties with real-time validation

### 5. Mode Toggle

Switch between fixed-width and CSV modes:

#### Fixed-Width Mode
- Uses `StartPosition` and `EndPosition` fields
- Optimized for fixed-width data files
- Automatic position calculation

#### CSV Mode
- Uses `Delimiter` field
- Clears position information
- Optimized for comma-separated values

## Step-by-Step Usage

### Method 1: Import from Excel

1. **Load Excel File**
   - Click "Load Excel Metadata"
   - Select your Excel file with field definitions
   - Choose the appropriate worksheet

2. **Review and Edit**
   - Review the imported field definitions
   - Edit field properties as needed
   - Check for validation errors/warnings

3. **Apply Auto-Fix** (if needed)
   - Click "Auto-Fix" to resolve common issues
   - Review the changes made

4. **Validate**
   - Click "Validate" to check all fields
   - Address any errors or warnings

5. **Export**
   - Click "Export" to generate the control file
   - Save in your preferred format

### Method 2: Start from Scratch

1. **Generate Default Fields**
   - Click "Start from Scratch"
   - 5 default fields will be created

2. **Add/Remove Fields**
   - Add new fields as needed
   - Remove unnecessary fields
   - Reorder fields by editing the Order column

3. **Configure Field Properties**
   - Set field names
   - Specify COBOL types
   - Oracle types will be auto-inferred
   - Set positions and lengths

4. **Validate and Export**
   - Run validation
   - Export the control file

### Method 3: Load Saved Definitions

1. **Load File**
   - Click "Load" to import saved field definitions
   - Supported formats: JSON, CSV

2. **Continue Editing**
   - Modify fields as needed
   - Apply validation

3. **Export**
   - Generate the final control file

## Field Properties

### Required Properties
- **Field Name:** Unique identifier for the field
- **Order:** Sequential order of the field in the record

### Optional Properties
- **Start Position:** Starting position in fixed-width files
- **End Position:** Ending position in fixed-width files
- **Length:** Field length
- **COBOL Type:** COBOL data type definition
- **SQL Type:** Oracle data type (auto-inferred from COBOL)
- **Nullable:** Whether the field can contain NULL values
- **Transform:** Data transformation expression
- **Default Value:** Default value for the field
- **Null If Value:** Value that should be treated as NULL
- **Enclosed By:** Character that encloses the field
- **Delimiter:** Field delimiter (for CSV mode)
- **Data Format:** Date/time format specification
- **Description:** Field description for documentation

## Export Formats

### 1. Oracle SQL*Loader Control File (.ctl)
Standard Oracle SQL*Loader control file format.

### 2. JSON (.json)
Complete field definitions with metadata for programmatic use.

### 3. CSV (.csv)
Tabular format for easy editing in spreadsheet applications.

### 4. SQL (.sql)
CREATE TABLE statements for direct database schema creation.

## Best Practices

### 1. COBOL Type Mapping
- Use specific COBOL types when possible
- Review auto-inferred Oracle types
- Adjust precision and scale as needed
- Consider performance implications of data types

### 2. Validation
- Run validation before export
- Address errors before warnings
- Use auto-fix for common issues
- Review validation results carefully

### 3. File Management
- Save work frequently
- Use descriptive file names
- Export in multiple formats
- Keep backup copies

### 4. Performance
- Large field sets (>100 fields) may impact performance
- Use validation sparingly on large datasets
- Consider breaking large definitions into smaller files

## Troubleshooting

### Common Issues

#### Type Mapping Problems
- **Issue:** Incorrect Oracle type inferred
- **Solution:** Manually specify SQL type or adjust COBOL type

#### Position Conflicts
- **Issue:** Overlapping field positions
- **Solution:** Use auto-fix or manually adjust positions

#### Validation Errors
- **Issue:** Fields marked as invalid
- **Solution:** Check field properties and fix according to error messages

#### Import Problems
- **Issue:** Excel file not loading correctly
- **Solution:** Check Excel format and ensure proper column headers

### Error Messages

#### "Field name is required"
- Ensure all fields have non-empty names

#### "Invalid SQL type"
- Use valid Oracle data types (CHAR, VARCHAR2, NUMBER, DATE, etc.)

#### "Position overlaps detected"
- Adjust start/end positions to eliminate overlaps

#### "Duplicate field name detected"
- Ensure field names are unique (case-insensitive)

## Advanced Features

### Custom Validation Rules
The system supports custom validation rules for specific requirements.

### Template System
Use predefined templates for common field definition patterns.

### Batch Processing
Process multiple files simultaneously for large-scale projects.

### Integration
Direct integration with Oracle databases for schema generation.

## Support

For additional support:
1. Check the documentation files
2. Review the sample templates
3. Use the validation system to identify issues
4. Export in multiple formats for different use cases

## Conclusion

The Oracle SQL*Loader Control File Generator provides a comprehensive solution for converting COBOL field definitions to Oracle SQL*Loader control files. With its advanced type mapping, validation, and export capabilities, it streamlines the data migration process while ensuring accuracy and reliability.

The system is designed to be both powerful and user-friendly, accommodating various COBOL patterns while providing clear feedback and suggestions for optimal Oracle type selection. 