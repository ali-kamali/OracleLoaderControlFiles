# Oracle SQL*Loader Control File Generator

A comprehensive Windows Forms (.NET 8, C#) desktop application for generating Oracle SQL*Loader .ctl files with support for both Excel metadata import and manual field definition.

## Features

### Core Functionality
- **Excel Metadata Import**: Import field definitions from Excel files containing metadata (not actual data)
- **Manual Entry Mode**: Create field definitions from scratch with an intuitive UI
- **COBOL to Oracle Type Mapping**: Automatic inference of Oracle SQL types from COBOL type definitions
- **Fixed-Width and CSV Modes**: Support for both fixed-width and delimited file formats
- **Live Preview**: Real-time preview of generated control files
- **Validation and Auto-Fix**: Comprehensive validation with automatic error correction

### Edge Case Handling
- **Missing SQL Types**: Automatically infer from COBOL types or default to CHAR
- **Missing Positions**: Calculate positions based on field order and previous field lengths
- **Overlapping Positions**: Highlight warnings without crashing the application
- **Duplicate Field Names**: Show warnings and allow user editing
- **Unknown Excel Columns**: Ignore and show non-blocking warnings

### UI Features
- **DataGridView Interface**: Edit fields directly in a grid with validation highlighting
- **Visual Validation**: Invalid rows/cells highlighted in red with tooltips
- **Mode Toggle**: Switch between fixed-width and CSV/delimited modes
- **Global Settings**: Configure table name, load mode, file paths, and other options
- **Export Options**: Save .ctl files and field configurations as JSON

## Requirements

- .NET 8.0 or later
- Windows 10/11
- Microsoft Open XML SDK (for Excel file processing)

## Installation

1. Clone or download the repository
2. Open the solution in Visual Studio 2022 or later
3. Restore NuGet packages
4. Build the solution
5. Run the application

## Usage

### Starting the Application
1. Launch the application
2. Choose between Excel import or manual entry mode

### Excel Import Mode
1. Click "Load Excel Metadata" to select an Excel file
2. Choose the appropriate worksheet from the dropdown
3. Review and edit imported field definitions
4. Configure global settings using the Settings button
5. Preview and export the control file

### Manual Entry Mode
1. Click "Start from Scratch" to begin with an empty field list
2. Use "Add Field" to create new field definitions
3. Edit field properties directly in the grid
4. Use "Validate" to check for issues
5. Use "Auto Fix" to automatically resolve common problems

### Field Management
- **Add Field**: Create a new field definition
- **Remove Field**: Delete selected field (requires row selection)
- **Edit Fields**: Modify properties directly in the grid
- **Reorder Fields**: Use the Order column to arrange fields

### Validation and Error Handling
- **Real-time Validation**: Fields are validated as you type
- **Visual Indicators**: 
  - Green: Valid fields
  - Yellow: Warnings
  - Red: Errors
- **Auto Fix**: Automatically resolve common issues
- **Manual Validation**: Click "Validate" to check all fields

### Mode Switching
- **Fixed Width**: For files with fixed-length fields
- **CSV/Delimited**: For comma-separated or delimited files
- **Toggle Mode**: Switch between modes using the toggle button

### Export Options
- **Preview**: View the generated control file before saving
- **Export .ctl**: Save the control file to disk
- **Data Preview**: Preview actual data using the field definitions
- **JSON Export**: Save field configuration for later use

## Field Definition Properties

### Required Properties
- **Field Name**: Unique identifier for the field
- **Order**: Sequential order of the field in the file

### Position Properties (Fixed Width Mode)
- **Start Position**: Beginning position of the field
- **End Position**: Ending position of the field
- **Length**: Field length (calculated automatically)

### Type Properties
- **COBOL Type**: Original COBOL type definition
- **SQL Type**: Oracle SQL data type (auto-inferred)
- **Nullable**: Whether the field can contain NULL values

### Advanced Properties
- **Transform**: Data transformation expression
- **Default Value**: Default value for the field
- **Null If Value**: Value to treat as NULL
- **Enclosed By**: Character that encloses the field (CSV mode)
- **Delimiter**: Field delimiter (CSV mode)
- **Data Format**: Date/time format specification
- **Description**: Field description for documentation

## Global Settings

### File Configuration
- **Table Name**: Target Oracle table name
- **Input File**: Path to the data file
- **Bad File**: Path for rejected records
- **Discard File**: Path for discarded records

### Load Options
- **Load Mode**: APPEND, REPLACE, INSERT, etc.
- **Encoding**: Character encoding (UTF8, etc.)
- **Max Errors**: Maximum number of errors before stopping
- **Skip Rows**: Number of header rows to skip

### Processing Options
- **Bind Size**: Size of bind arrays
- **Rows**: Number of rows per commit
- **Use Direct Path**: Enable direct path loading
- **Trailing Null Columns**: Handle trailing null columns

### CSV/Delimited Options
- **Field Terminator**: Character that separates fields
- **Enclosed By**: Character that encloses fields
- **Optionally Enclosed**: Whether fields are optionally enclosed
- **Trim Option**: How to handle whitespace

## COBOL to Oracle Type Mapping

The application includes comprehensive COBOL to Oracle type mapping:

### Numeric Types
- `PIC 9(n)` → `NUMBER(n)`
- `PIC S9(n)` → `NUMBER(n)` (signed)
- `PIC 9(n)V9(m)` → `NUMBER(n+m,m)` (decimal)

### Character Types
- `PIC X(n)` → `CHAR(n)` or `VARCHAR2(n)`
- `PIC A(n)` → `CHAR(n)`

### Date Types
- `PIC 9(8)` → `DATE` (with format specification)
- `PIC X(10)` → `DATE` (with format specification)

### Special Types
- `PIC 9(15)V99` → `NUMBER(17,2)` (currency)
- `PIC S9(10)V99` → `NUMBER(12,2)` (signed currency)

## Error Handling

### Validation Errors
- Missing required fields
- Invalid data types
- Position overlaps
- Duplicate field names

### Warnings
- Missing SQL types (auto-inferred)
- Missing positions (auto-calculated)
- Unknown Excel columns
- Potential data type mismatches

### Auto-Fix Capabilities
- Infer missing SQL types from COBOL types
- Calculate missing positions based on field order
- Suggest appropriate data formats
- Resolve common naming conflicts

## File Formats

### Excel Metadata Format
The Excel file should contain columns for field definitions:
- Field Name
- Order
- Start Position (optional)
- End Position (optional)
- Length (optional)
- COBOL Type
- SQL Type (optional)
- Other properties as needed

### Generated Control File Format
Standard Oracle SQL*Loader control file format:
```sql
LOAD DATA
INFILE 'datafile.dat'
BADFILE 'badfile.bad'
DISCARDFILE 'discardfile.dsc'
APPEND
INTO TABLE table_name
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    field1 POSITION(1:10) CHAR(10),
    field2 POSITION(11:20) NUMBER(10),
    field3 POSITION(21:30) DATE "YYYY-MM-DD"
)
```

## Troubleshooting

### Common Issues
1. **Excel Import Fails**: Ensure the Excel file contains the expected column headers
2. **Validation Errors**: Use the Auto Fix button to resolve common issues
3. **Position Overlaps**: Check field positions and use Auto Fix to recalculate
4. **Type Mapping Issues**: Verify COBOL type format and check the mapping guide

### Performance Tips
- Use Direct Path loading for large files
- Adjust Bind Size and Rows based on available memory
- Consider using external tables for very large datasets

## Development

### Project Structure
```
ControlFileGenerator.WinForms/
├── Forms/                 # Windows Forms UI
├── Models/               # Data models
├── Services/             # Business logic services
└── Resources/            # Templates and resources
```

### Key Services
- **ExcelMetadataParser**: Handles Excel file import
- **CobolTypeMapper**: Maps COBOL types to Oracle types
- **EdgeCaseHandler**: Manages validation and error handling
- **ControlFileGenerator**: Generates the final .ctl file
- **ConfigurationService**: Manages application settings

### Extending the Application
- Add new COBOL type mappings in `CobolTypeMapper.cs`
- Implement additional validation rules in `EdgeCaseHandler.cs`
- Create new export formats in `FieldDefinitionExporter.cs`
- Add new UI features in the Forms directory

## License

This project is provided as-is for educational and development purposes.

## Support

For issues and questions:
1. Check the validation messages in the application
2. Review the generated control file syntax
3. Consult Oracle SQL*Loader documentation
4. Check the COBOL to Oracle mapping guide included with the application 