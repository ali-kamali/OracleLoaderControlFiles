# Oracle SQL*Loader Control File Generator

A Windows Forms desktop application built with .NET 8 that generates Oracle SQL*Loader control files from Excel metadata definitions.

## Features

- **Excel Import**: Load field definitions from Excel files with flexible column mapping
- **Field Editor**: Edit field properties with real-time preview
- **Settings Management**: Configure Oracle table settings, file references, and advanced options
- **Control File Preview**: Preview generated control files before export
- **Export**: Save control files in .ctl format
- **Validation**: Built-in validation for field definitions and control file syntax

## Getting Started

### Prerequisites

- Windows 10/11
- .NET 8 Runtime
- Excel files (.xlsx, .xls) with field definitions

### Installation

1. Download the latest release
2. Extract the ZIP file
3. Run `ControlFileGenerator.WinForms.exe`

### Excel File Format

The application expects Excel files with the following columns (column names are flexible):

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

### Sample Excel Template

A sample CSV template is included in `Resources/Templates/sample_field_definitions.csv` that you can import into Excel.

## Usage

### 1. Load Excel Metadata

1. Click "Load Excel Metadata"
2. Select your Excel file
3. Choose the worksheet containing field definitions
4. The application will parse and display the fields in a grid

### 2. Configure Settings

1. Click "Settings" to open the configuration dialog
2. Set Oracle table name and load mode
3. Configure file paths (INFILE, BADFILE, DISCARDFILE)
4. Set advanced options (DIRECT, ERRORS, BINDSIZE, etc.)

### 3. Preview Control File

1. Click "Preview" to see the generated control file
2. Review the syntax and field mappings
3. Use Ctrl+C to copy to clipboard or save directly

### 4. Export Control File

1. Click "Export .ctl" to save the control file
2. Choose a location and filename
3. The file will be saved in Oracle SQL*Loader format

## Generated Control File Example

```sql
-- Oracle SQL*Loader Control File
-- Generated on: 2024-01-15 10:30:00
-- Table: EMPLOYEES

LOAD DATA
INFILE 'employees.dat'
BADFILE 'employees.bad'
DISCARDFILE 'employees.dsc'
INTO TABLE EMPLOYEES
APPEND
TRAILING NULLCOLS
(
  EMPNO      POSITION(1:6)     INTEGER EXTERNAL,
  ENAME      POSITION(7:26)    CHAR NULLIF ENAME=BLANKS "UPPER(:ENAME)",
  SAL        POSITION(27:33)   DECIMAL EXTERNAL NULLIF SAL="99999" ":SAL * 100",
  HIREDATE   POSITION(34:41)   DATE "YYYYMMDD"
)
```

## Configuration

The application saves configuration in:
- `%APPDATA%\OracleControlFileGenerator\config.json`
- `%APPDATA%\OracleControlFileGenerator\logs\`

### Settings Categories

#### Oracle Table Settings
- **Table Name**: Target Oracle table
- **Load Mode**: APPEND, REPLACE, INSERT, TRUNCATE
- **Trailing Null Columns**: Enable TRAILING NULLCOLS

#### File References
- **Input File**: Data file path
- **Bad File**: Rejected records file
- **Discard File**: Discarded records file
- **Encoding**: Character encoding

#### Advanced Options
- **Direct Path**: Enable DIRECT=TRUE
- **Max Errors**: Maximum allowed errors
- **Bind Size**: Memory allocation
- **Rows**: Rows per commit

#### Field Specifications
- **Field Terminator**: Delimiter character
- **Enclosed By**: Quote character
- **Trim Option**: LRTRIM, LTRIM, RTRIM, NOTRIM

## Troubleshooting

### Common Issues

1. **Excel file not loading**: Ensure the file has proper headers and data
2. **Field validation errors**: Check field names, positions, and data types
3. **Control file syntax errors**: Verify Oracle table name and field mappings

### Logs

Application logs are stored in `%APPDATA%\OracleControlFileGenerator\logs\` and can help diagnose issues.

## Development

### Building from Source

```bash
git clone <repository>
cd ControlFileGenerator.WinForms/ControlFileGenerator.WinForms
dotnet build
dotnet run
```

### Dependencies

- .NET 8.0
- EPPlus 8.0.8 (Excel processing)
- Windows Forms

### Project Structure

```
ControlFileGenerator.WinForms/
├── Forms/
│   ├── MainForm.cs              # Main application window
│   ├── SettingsForm.cs          # Configuration dialog
│   ├── PreviewForm.cs           # Control file preview
│   └── FieldEditorForm.cs       # Field editing dialog
├── Models/
│   ├── FieldDefinition.cs       # Field metadata model
│   └── LoaderConfig.cs          # Configuration model
├── Services/
│   ├── ExcelMetadataParser.cs   # Excel file parsing
│   ├── ControlFileGenerator.cs  # Control file generation
│   ├── PositionCalculator.cs    # Position calculations
│   ├── ConfigurationService.cs  # Settings management
│   └── LoggingService.cs        # Application logging
└── Resources/
    └── Templates/               # Sample templates
```

## License

This project is licensed under the MIT License.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Support

For issues and questions:
1. Check the troubleshooting section
2. Review application logs
3. Create an issue in the repository 