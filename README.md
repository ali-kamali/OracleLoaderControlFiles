# Oracle SQL*Loader Control File Generator

A comprehensive Windows Forms (.NET 8, C#) desktop application for generating Oracle SQL*Loader .ctl files with support for both Excel metadata import and manual field definition.

## 🚀 Features

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

## 📋 Requirements

- .NET 8.0 or later
- Windows 10/11
- EPPlus library (for Excel file processing)

## 🛠️ Installation & Setup

### Prerequisites
1. Install .NET 8.0 SDK from [Microsoft](https://dotnet.microsoft.com/download/dotnet/8.0)
2. Visual Studio 2022 or later (recommended)

### Building from Source
```bash
# Clone the repository
git clone <repository-url>
cd Oracle

# Build the solution
dotnet build Oracle.sln

# Run the application
cd ControlFileGenerator.WinForms/ControlFileGenerator.WinForms
dotnet run
```

### Running the Application
1. Navigate to the project directory
2. Run `dotnet run` or open the solution in Visual Studio
3. The application will launch with a modern Windows Forms interface

## 📖 Quick Start Guide

### Option 1: Excel Import Mode
1. **Prepare Excel File**: Create an Excel file with field definitions (see templates)
2. **Load Excel**: Click "Load Excel Metadata" and select your file
3. **Choose Worksheet**: Select the appropriate worksheet from dropdown
4. **Review & Edit**: Modify field properties as needed
5. **Configure Settings**: Set table name, file paths, and options
6. **Export**: Generate and save the .ctl file

### Option 2: Manual Entry Mode
1. **Start Fresh**: Click "Start from Scratch"
2. **Add Fields**: Use "Add Field" button to create field definitions
3. **Configure Properties**: Set field names, types, positions, etc.
4. **Validate**: Click "Validate" to check for issues
5. **Auto Fix**: Use "Auto Fix" to resolve common problems
6. **Export**: Generate and save the .ctl file

## 🎯 Key Features in Detail

### Excel Metadata Import
- Supports .xlsx and .xls formats
- Flexible column mapping
- Multiple worksheet support
- Automatic type inference
- Error handling for malformed files

### Manual Field Definition
- Intuitive grid-based editing
- Real-time validation
- Auto-completion suggestions
- Bulk operations support
- Undo/redo functionality

### COBOL to Oracle Type Mapping
Comprehensive mapping including:
- **Numeric**: `PIC 9(n)` → `NUMBER(n)`
- **Signed**: `PIC S9(n)` → `NUMBER(n)`
- **Decimal**: `PIC 9(n)V9(m)` → `NUMBER(n+m,m)`
- **Character**: `PIC X(n)` → `CHAR(n)` or `VARCHAR2(n)`
- **Date**: `PIC 9(8)` → `DATE` with format specification

### Validation System
- **Real-time validation**: Fields validated as you type
- **Visual indicators**: Color-coded status (Green/Yellow/Red)
- **Comprehensive checks**: Type, position, naming, and syntax validation
- **Auto-fix capabilities**: Automatic resolution of common issues

### Mode Support
- **Fixed Width**: For files with fixed-length fields
- **CSV/Delimited**: For comma-separated or delimited files
- **Easy switching**: Toggle between modes with one click
- **Mode-specific validation**: Different rules for each mode

## 📁 Project Structure

```
Oracle/
├── ControlFileGenerator.WinForms/
│   └── ControlFileGenerator.WinForms/
│       ├── Forms/                 # Windows Forms UI
│       │   ├── MainForm.cs        # Main application window
│       │   ├── SettingsForm.cs    # Configuration dialog
│       │   ├── PreviewForm.cs     # Control file preview
│       │   ├── DataPreviewForm.cs # Data preview window
│       │   └── FieldEditorForm.cs # Field editing dialog
│       ├── Models/                # Data models
│       │   ├── FieldDefinition.cs # Field metadata model
│       │   └── LoaderConfig.cs    # Configuration model
│       ├── Services/              # Business logic services
│       │   ├── ExcelMetadataParser.cs   # Excel file parsing
│       │   ├── CobolTypeMapper.cs       # COBOL to Oracle mapping
│       │   ├── EdgeCaseHandler.cs       # Validation and error handling
│       │   ├── ControlFileGenerator.cs  # Control file generation
│       │   ├── ConfigurationService.cs  # Settings management
│       │   ├── LoggingService.cs        # Application logging
│       │   ├── PositionCalculator.cs    # Position calculations
│       │   ├── FieldDefinitionExporter.cs # Export functionality
│       │   ├── BatchProcessor.cs        # Batch processing
│       │   └── TemplateManager.cs       # Template management
│       └── Resources/
│           └── Templates/         # Sample templates
│               ├── sample_field_definitions.csv
│               └── sample_field_definitions_enhanced.csv
├── README.md                      # This file
├── USAGE_GUIDE.md                 # Detailed usage instructions
├── COBOL_TO_ORACLE_MAPPING_GUIDE.md # Type mapping reference
└── Oracle.sln                     # Solution file
```

## 🔧 Configuration

### Global Settings
- **Table Configuration**: Table name, load mode, trailing null columns
- **File References**: Input file, bad file, discard file paths
- **Processing Options**: Bind size, rows per commit, max errors
- **CSV Options**: Field terminators, enclosure characters, trim options

### Field Properties
- **Basic**: Field name, order, COBOL type, SQL type
- **Position**: Start position, end position, length (fixed width)
- **Advanced**: Transformations, default values, null conditions
- **Format**: Date formats, number precision, character encoding

## 📊 Sample Templates

The application includes sample templates to get you started:

### Basic Template (`sample_field_definitions.csv`)
Simple employee data example with common field types.

### Enhanced Template (`sample_field_definitions_enhanced.csv`)
Comprehensive example with 20 fields including:
- Employee information
- Address details
- System fields
- Various data types and transformations

## 🚨 Error Handling

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

## 📝 Generated Control File Example

```sql
LOAD DATA
INFILE 'employees.dat'
BADFILE 'employees.bad'
DISCARDFILE 'employees.dsc'
APPEND
INTO TABLE EMPLOYEES
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    EMPNO      POSITION(1:6)     INTEGER EXTERNAL,
    ENAME      POSITION(7:26)    CHAR NULLIF ENAME=BLANKS "UPPER(:ENAME)",
    SAL        POSITION(27:33)   DECIMAL EXTERNAL NULLIF SAL="99999" ":SAL * 100",
    HIREDATE   POSITION(34:41)   DATE "YYYYMMDD",
    DEPTNO     POSITION(42:44)   INTEGER EXTERNAL,
    JOB        POSITION(45:54)   CHAR "UPPER(:JOB)",
    MGR        POSITION(55:60)   INTEGER EXTERNAL,
    COMM       POSITION(61:68)   DECIMAL EXTERNAL
)
```

## 🛠️ Development

### Building the Project
```bash
# Build the solution
dotnet build Oracle.sln

# Run tests (if available)
dotnet test

# Create release build
dotnet publish -c Release
```

### Key Services
- **ExcelMetadataParser**: Handles Excel file import with error handling
- **CobolTypeMapper**: Comprehensive COBOL to Oracle type mapping
- **EdgeCaseHandler**: Manages validation and error handling
- **ControlFileGenerator**: Generates the final .ctl file
- **ConfigurationService**: Manages application settings and persistence

### Extending the Application
- Add new COBOL type mappings in `CobolTypeMapper.cs`
- Implement additional validation rules in `EdgeCaseHandler.cs`
- Create new export formats in `FieldDefinitionExporter.cs`
- Add new UI features in the Forms directory

## 📚 Documentation

- **[Usage Guide](USAGE_GUIDE.md)**: Detailed step-by-step instructions
- **[COBOL to Oracle Mapping Guide](COBOL_TO_ORACLE_MAPPING_GUIDE.md)**: Complete type mapping reference
- **[Oracle Loader Reference](oracle_loader_reference.md)**: SQL*Loader syntax reference
- **[Oracle Data Types Reference](oracle_data_types_reference.md)**: Oracle data type documentation

## 🐛 Troubleshooting

### Common Issues
1. **Excel Import Fails**: Check file format and column headers
2. **Validation Errors**: Use Auto Fix button for common issues
3. **Position Overlaps**: Verify field positions and use Auto Fix
4. **Type Mapping Issues**: Check COBOL type format

### Performance Tips
- Use Direct Path loading for large files
- Adjust Bind Size and Rows based on available memory
- Consider using external tables for very large datasets

### Getting Help
1. Check the validation messages in the application
2. Review the generated control file syntax
3. Consult Oracle SQL*Loader documentation
4. Check the COBOL to Oracle mapping guide

## 📄 License

This project is provided as-is for educational and development purposes.

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## 📞 Support

For issues and questions:
1. Check the troubleshooting section
2. Review application logs
3. Consult the usage guide
4. Create an issue in the repository

---

**Built with ❤️ using .NET 8 and Windows Forms** 