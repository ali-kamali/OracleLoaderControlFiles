# Oracle SQL*Loader Control File Generator

A Windows Forms application built with .NET 8 that generates Oracle SQL*Loader control files from Excel metadata definitions.

## Features

- **Excel Import**: Load field definitions from Excel files with flexible column mapping
- **Field Mapping**: Edit field properties including positions, data types, and transformations
- **Position Calculation**: Automatic calculation of field positions for fixed-width files
- **Loader Settings**: Configure SQL*Loader options like table name, load mode, and file paths
- **Control File Generation**: Generate valid SQL*Loader control files for both fixed-width and delimited formats
- **Preview & Export**: Preview generated control files and export to .ctl files
- **Validation**: Comprehensive validation of field definitions and loader settings

## Requirements

- Windows 10/11
- .NET 8 Runtime
- Excel files (.xlsx, .xls) for field definitions

## Installation

1. Download the latest release
2. Extract the files to a directory
3. Run `ControlFileGenerator.WinForms.exe`

## Usage

### 1. Load Excel Metadata

1. Click **"Load Excel"** to select an Excel file containing field definitions
2. Choose the appropriate worksheet from the dropdown
3. The application will automatically parse the Excel data and display field mappings

### 2. Expected Excel Columns

The application supports flexible column mapping. Here are the supported column names:

| Column Name | Required | Description |
|-------------|----------|-------------|
| Field Name | ✅ Yes | Oracle column name |
| Order | ❌ | Order of field in file |
| Start Position | ❌ | For fixed-width files |
| End Position | ❌ | Used to derive length |
| Length | ❌ | Used if no end position |
| COBOL Type | ❌ | Optional, used for type mapping |
| SQL Type | ❌ | Optional, CHAR/NUMBER/DATE etc. |
| Nullable | ❌ | YES/NO |
| Transform | ❌ | SQL expressions |
| Default Value | ❌ | Constant |
| Null If Value | ❌ | Used in NULLIF clause |
| Enclosed By | ❌ | For delimited fields |
| Delimiter | ❌ | Field-level delimiter (CSV) |
| Data Format | ❌ | For dates/numbers |
| Description | ❌ | Ignored during generation |

### 3. Configure Loader Settings

1. Click **"Loader Settings"** to configure SQL*Loader options
2. Set the target table name and load mode
3. Configure file paths for input, bad, and discard files
4. Set advanced options like character set, field terminators, etc.

### 4. Generate Control File

1. Click **"Preview Control File"** to see the generated control file
2. Review the content and make any necessary adjustments
3. Click **"Export .ctl File"** to save the control file

## Sample Excel Template

A sample CSV template is included in `Resources/Templates/sample_field_definitions.csv` that you can import into Excel as a starting point.

## Generated Control File Example

```sql
-- Oracle SQL*Loader Control File
-- Generated on: 2025-01-27 10:30:00
-- Table: EMPLOYEES

LOAD DATA
INFILE 'employees.dat'
BADFILE 'employees.bad'
DISCARDFILE 'employees.dsc'
INTO TABLE EMPLOYEES
APPEND
(
  EMPLOYEE_ID   POSITION(1:6)     NUMBER EXTERNAL,
  FIRST_NAME    POSITION(7:26)    VARCHAR2 "UPPER(:FIRST_NAME)" NULLIF FIRST_NAME=BLANKS,
  LAST_NAME     POSITION(27:46)   VARCHAR2 "UPPER(:LAST_NAME)" NULLIF LAST_NAME=BLANKS,
  EMAIL         POSITION(47:76)   VARCHAR2 "LOWER(:EMAIL)" NULLIF EMAIL=BLANKS,
  HIRE_DATE     POSITION(77:84)   DATE "YYYYMMDD" NULLIF HIRE_DATE="00000000",
  SALARY        POSITION(85:91)   NUMBER EXTERNAL ":SALARY * 100" NULLIF SALARY="9999999",
  DEPARTMENT_ID POSITION(92:94)   NUMBER EXTERNAL NULLIF DEPARTMENT_ID="999",
  MANAGER_ID    POSITION(95:100)  NUMBER EXTERNAL NULLIF MANAGER_ID="999999",
  STATUS        POSITION(101:101) CHAR "UPPER(:STATUS)" DEFAULTIF STATUS=BLANKS "A" NULLIF STATUS=BLANKS,
  CREATED_DATE  POSITION(102:109) DATE "YYYYMMDD" NULLIF CREATED_DATE="00000000"
)
```

## Features in Detail

### Position Calculation

- Automatically calculates field positions based on order and length
- Handles gaps and overlaps in field positioning
- Supports both fixed-width and delimited formats

### Data Type Mapping

- Maps COBOL types to Oracle data types
- Supports common Oracle data types: CHAR, VARCHAR2, NUMBER, DATE, TIMESTAMP
- Handles external numeric formats

### Transformations

- Supports SQL expressions for field transformations
- Common transformations: UPPER(), LOWER(), arithmetic operations
- Default values and NULLIF clauses

### Validation

- Validates field definitions for completeness
- Checks for position overlaps and gaps
- Validates loader configuration settings
- Ensures generated control file syntax is correct

## Troubleshooting

### Common Issues

1. **Excel parsing errors**: Ensure your Excel file has a header row with recognizable column names
2. **Position calculation errors**: Check that field lengths and positions are consistent
3. **Control file generation errors**: Verify that required fields (table name, input file) are set

### Validation Messages

The application provides detailed validation messages to help identify and fix issues:

- Missing required fields
- Invalid numeric values
- Position overlaps
- Configuration errors

## Development

### Project Structure

```
ControlFileGenerator.WinForms/
├── Models/
│   ├── FieldDefinition.cs
│   └── LoaderConfig.cs
├── Services/
│   ├── ExcelMetadataParser.cs
│   ├── PositionCalculator.cs
│   └── ControlFileGenerator.cs
├── Forms/
│   ├── MainForm.cs
│   ├── SettingsForm.cs
│   └── PreviewForm.cs
└── Resources/
    └── Templates/
```

### Building from Source

1. Clone the repository
2. Open the solution in Visual Studio 2022
3. Restore NuGet packages
4. Build the solution
5. Run the application

### Dependencies

- .NET 8.0
- EPPlus (for Excel file parsing)
- Windows Forms

## License

This project is licensed under the MIT License.

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for bugs and feature requests. 