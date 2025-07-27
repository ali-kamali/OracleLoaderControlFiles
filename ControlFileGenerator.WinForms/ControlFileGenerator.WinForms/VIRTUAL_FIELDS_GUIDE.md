# Virtual Fields Guide

## Overview

Virtual fields are artificial fields that do not exist in the original data file but are included in the SQL*Loader control file. They allow you to add custom fields like batch IDs, timestamps, or filler fields to your data loading process.

## Features

### Virtual Field Checkbox
- A new "Virtual" checkbox column has been added to the DataGridView
- When checked, the field is treated as a virtual field
- Virtual fields bypass position and delimiter validation

### Two Generation Methods

#### 1. Constant Values
If a Default Value is provided:
```sql
FIELD_NAME CONSTANT 'default_value'
```

Example:
- Field Name: `MIS_DATE`
- Default Value: `2024-01-15`
- Output: `MIS_DATE CONSTANT '2024-01-15'`

#### 2. SQL Expressions
If a Transform is provided:
```sql
FIELD_NAME "sql_expression"
```

Example:
- Field Name: `BATCH_ID`
- Transform: `:BATCH_ID_SEQ.NEXTVAL`
- Output: `BATCH_ID ":BATCH_ID_SEQ.NEXTVAL"`

### FILLER Fields
Special handling for fields starting with "FILLER":
- Can optionally have position and length for file space reservation
- Output format: `FILLER1 POSITION(27:30) FILLER`
- If no position is specified, outputs as: `FILLER1 FILLER`

## Validation Rules

### Virtual Fields
- Must have either Default Value OR Transform expression
- Position, Length, Order, and Delimiter fields are ignored
- No position validation or overlap checking

### Non-Virtual Fields
- Fixed-width mode: Requires valid Position or Length
- CSV mode: Requires valid Order
- Normal validation applies

## UI Behavior

### When Virtual Field is Checked
- Position-related columns become read-only
- Delimiter columns become read-only
- Position values are cleared automatically
- Default SQL type is set to CHAR if not specified

### When Virtual Field is Unchecked
- Position and delimiter columns become editable based on mode
- Normal validation rules apply

## Examples

### Example 1: Mission Date Field
```
Field Name: MIS_DATE
Is Virtual: ✓
Default Value: 2024-01-15
SQL Type: CHAR
```
Output: `MIS_DATE CONSTANT '2024-01-15'`

### Example 2: Auto-Generated Batch ID
```
Field Name: BATCH_ID
Is Virtual: ✓
Transform: :BATCH_ID_SEQ.NEXTVAL
SQL Type: NUMBER
```
Output: `BATCH_ID ":BATCH_ID_SEQ.NEXTVAL"`

### Example 3: FILLER Field with Position
```
Field Name: FILLER1
Is Virtual: ✓
Start Position: 273
End Position: 280
SQL Type: CHAR
```
Output: `FILLER1 POSITION(273:280) FILLER`

### Example 4: FILLER Field without Position
```
Field Name: FILLER1
Is Virtual: ✓
SQL Type: CHAR
```
Output: `FILLER1 FILLER`

## Template Files

The sample template files have been updated to include virtual field examples:
- `sample_field_definitions.csv` - Basic examples
- `sample_field_definitions_enhanced.csv` - Advanced examples with various virtual field types

## Best Practices

1. **Use descriptive names** for virtual fields (e.g., `MIS_DATE`, `BATCH_ID`, `FILLER1`)
2. **Choose appropriate SQL types** for your virtual fields
3. **Use CONSTANT for static values** and Transform for dynamic expressions
4. **Reserve file space** with positioned FILLER fields when needed
5. **Validate your control file** after adding virtual fields

## Common Use Cases

- **Batch processing**: Add batch IDs or timestamps
- **Data lineage**: Add source file information
- **Audit trails**: Add user IDs or creation dates
- **File formatting**: Use FILLER fields to reserve space
- **Data transformation**: Add calculated fields 