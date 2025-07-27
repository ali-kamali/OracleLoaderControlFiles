# COBOL to Oracle Type Mapping System

## Overview

This document describes the comprehensive COBOL to Oracle type mapping system implemented in the Oracle SQL*Loader Control File Generator. The system includes automatic type inference, edge case handling, validation, and export capabilities.

## COBOL → Oracle Type Mapping Table

### Basic Numeric Types
| COBOL Type | Oracle Type | Description |
|------------|-------------|-------------|
| `PIC 9` | `NUMBER(1)` | Single digit |
| `PIC 9(n)` | `NUMBER(n)` | n-digit number (1-38) |
| `PIC S9` | `NUMBER(1)` | Signed single digit |
| `PIC S9(n)` | `NUMBER(n)` | Signed n-digit number |

### Decimal Types
| COBOL Type | Oracle Type | Description |
|------------|-------------|-------------|
| `PIC 9(n)V99` | `NUMBER(n+2,2)` | Decimal with 2 decimal places |
| `PIC 9(n)V999` | `NUMBER(n+3,3)` | Decimal with 3 decimal places |
| `PIC S9(n)V99` | `NUMBER(n+2,2)` | Signed decimal with 2 decimal places |
| `PIC S9(n)V999` | `NUMBER(n+3,3)` | Signed decimal with 3 decimal places |

### Character Types
| COBOL Type | Oracle Type | Description |
|------------|-------------|-------------|
| `PIC X` | `CHAR(1)` | Single character |
| `PIC X(n)` | `CHAR(n)` | Fixed-length string (n ≤ 10) |
| `PIC X(n)` | `VARCHAR2(n)` | Variable-length string (n > 10) |

### Date Types
| COBOL Type | Oracle Type | Description |
|------------|-------------|-------------|
| `PIC 9(6)` | `DATE` | YYMMDD format |
| `PIC 9(8)` | `DATE` | YYYYMMDD format |
| `PIC 9(7)` | `DATE` | YYYYDDD (Julian) format |

### Special Types
| COBOL Type | Oracle Type | Description |
|------------|-------------|-------------|
| `COMP` | `NUMBER` | Computational |
| `COMP-1` | `BINARY_FLOAT` | Single precision |
| `COMP-2` | `BINARY_DOUBLE` | Double precision |
| `COMP-3` | `NUMBER` | Packed decimal |
| `COMP-4` | `NUMBER` | Binary |
| `COMP-5` | `NUMBER` | Native binary |

## Edge Case Guidelines

### Case 1: Missing SQL Type
**Expected Behavior:** Infer using COBOL or default to CHAR
```csharp
// If SQL type is missing, infer from COBOL type
if (string.IsNullOrWhiteSpace(field.SqlType))
{
    field.SqlType = CobolTypeMapper.MapCobolToOracle(field.CobolType);
}
```

### Case 2: Missing Start/End/Length
**Expected Behavior:** Use Order + prior lengths, or fallback
```csharp
// Auto-calculate positions based on order and previous fields
EdgeCaseHandler.ApplyEdgeCaseHandling(fields);
```

### Case 3: Overlapping Positions
**Expected Behavior:** Show warning, don't crash
```csharp
var warnings = EdgeCaseHandler.DetectOverlappingPositions(fields);
// Display warnings to user but allow continuation
```

### Case 4: Duplicate Field Names
**Expected Behavior:** Warn, allow edit
```csharp
var warnings = EdgeCaseHandler.DetectDuplicateFieldNames(fields);
// Show warnings but don't prevent editing
```

### Case 5: Unknown Column in Excel
**Expected Behavior:** Ignore, show as warning
```csharp
var warnings = EdgeCaseHandler.DetectUnknownColumns(excelColumns, fields);
// Log unknown columns but continue processing
```

## Validation System

### Field-Level Validation
- **Field Name:** Required, non-empty
- **SQL Type:** Must be valid Oracle data type
- **Length:** Must be positive if specified
- **Positions:** Start must be ≤ End if both specified

### Cross-Field Validation
- **Duplicate Names:** Case-insensitive detection
- **Position Overlaps:** Detect overlapping field positions
- **Order Consistency:** Validate field ordering

### Validation Status Highlighting
- **Green:** Valid fields
- **Yellow:** Fields with warnings
- **Red:** Fields with errors

## Auto-Fix Capabilities

### Automatic Position Calculation
```csharp
// Calculate missing positions based on order and length
CobolTypeMapper.AutoCalculatePositions(fields);
```

### Type Inference
```csharp
// Infer Oracle types from COBOL types
field.SqlType = CobolTypeMapper.MapCobolToOracle(field.CobolType);
```

### Default Value Assignment
```csharp
// Assign sensible defaults for missing values
field.SqlType = field.SqlType ?? "VARCHAR2(255)";
field.Nullable = field.Nullable ?? true;
```

## Mode Toggle: Fixed-Width vs CSV

### Fixed-Width Mode
- Uses `StartPosition` and `EndPosition`
- Calculates positions automatically
- Optimized for fixed-width data files

### CSV Mode
- Uses `Delimiter` field
- Clears position information
- Optimized for comma-separated values

## Export/Import Functionality

### Supported Formats
1. **JSON:** Complete field definitions with metadata
2. **CSV:** Tabular format for easy editing
3. **SQL:** CREATE TABLE statements
4. **Control File:** Oracle SQL*Loader format

### Export Example (JSON)
```json
{
  "exportDate": "2024-01-15T10:30:00",
  "fieldDefinitions": [
    {
      "fieldName": "CUSTOMER_ID",
      "order": 1,
      "startPosition": 1,
      "endPosition": 10,
      "length": 10,
      "cobolType": "PIC 9(10)",
      "sqlType": "NUMBER(10)",
      "nullable": false
    }
  ],
  "metadata": {
    "totalFields": 1,
    "hasErrors": false,
    "hasWarnings": false
  }
}
```

### Import Capabilities
- Load field definitions from saved files
- Preserve all field properties
- Apply validation on import

## Starting from Scratch

### Default Field Generation
```csharp
// Generate 5 default fields
var fields = EdgeCaseHandler.GenerateFieldsFromScratch(5);
```

### Field Management
- **Add Field:** Create new field with auto-incremented order
- **Remove Field:** Delete selected field and reorder remaining
- **Edit Field:** Modify field properties with real-time validation

## Error Handling and Logging

### Comprehensive Error Messages
- Clear, descriptive error messages
- Field-specific error reporting
- Suggested fixes for common issues

### Logging Levels
- **Info:** Normal operations
- **Warning:** Non-critical issues
- **Error:** Critical problems

## Best Practices

### COBOL Type Mapping
1. Use specific COBOL types when possible
2. Leverage automatic type inference
3. Review suggested Oracle types
4. Adjust precision and scale as needed

### Validation
1. Run validation before export
2. Address errors before warnings
3. Use auto-fix for common issues
4. Review validation results carefully

### File Management
1. Save work frequently
2. Use descriptive file names
3. Export in multiple formats
4. Keep backup copies

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

### Performance Considerations
- Large field sets (>100 fields) may impact performance
- Use validation sparingly on large datasets
- Consider breaking large definitions into smaller files

## Future Enhancements

### Planned Features
1. **Advanced Type Mapping:** Support for more COBOL patterns
2. **Batch Processing:** Handle multiple files simultaneously
3. **Template System:** Predefined field definition templates
4. **Integration:** Direct database schema generation
5. **Validation Rules:** Custom validation rule definition

### Extensibility
- Plugin architecture for custom type mappers
- Custom validation rule support
- Export format extensibility
- Integration with external systems

## Conclusion

The COBOL to Oracle type mapping system provides a robust, user-friendly solution for converting COBOL field definitions to Oracle SQL*Loader control files. With comprehensive edge case handling, validation, and export capabilities, it streamlines the process of data migration and integration projects.

The system is designed to be both powerful and flexible, accommodating various COBOL patterns while providing clear feedback and suggestions for optimal Oracle type selection. 