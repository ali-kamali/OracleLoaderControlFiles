# How to Choose Which Fields to Apply Smart Null Values

## Overview

The Oracle Control File Generator provides **field-specific** smart null value patterns, allowing users to choose which **individual string fields** should have smart null value patterns applied. This feature is specifically designed for **string/text fields** where trimming and empty data handling makes the most sense.

## Method 1: Individual Field Selection (Primary Method)

### **Step-by-Step Process:**
1. **Click on a specific string field row** in the DataGridView
2. **Click on the "Null If Value" column** for that field
3. **Type or select** the smart pattern (e.g., `TRIM_IF_NOT_EMPTY`)
4. **Press Enter** to apply to that specific field

### **Best For:**
- Applying different patterns to different string fields
- Fine-tuning specific field behavior
- Precise control over null value handling

### **Example:**
```
Field Name: customer_name → Null If Value: TRIM_IF_NOT_EMPTY
Field Name: email → Null If Value: EMPTY_OR_WHITESPACE
Field Name: phone → Null If Value: EMPTY_OR_NULL
```

## Method 2: Smart Null Value Buttons (String Fields Only)

### **Step-by-Step Process:**
1. **Select a string field** (VARCHAR2, CHAR, CLOB) in the DataGridView
2. **Click one of the smart null buttons** in the "String Field Smart Null" panel:
   - **TRIM_IF_NOT_EMPTY** - Trims data and handles empty results
   - **EMPTY_OR_WHITESPACE** - Handles empty strings and whitespace
   - **EMPTY_OR_NULL** - Handles empty strings and 'NULL' values

### **Best For:**
- Quick application of common string field patterns
- Visual confirmation of which pattern is being applied
- Consistent string field handling

### **What Happens:**
- Only works on string fields (VARCHAR2, CHAR, CLOB)
- Shows error message if non-string field is selected
- Immediately applies the pattern to the selected field

## Method 3: Context Menu for Selected String Field

### **Step-by-Step Process:**
1. **Right-click on a string field row**
2. **Select "Apply to String Field"**
3. **Choose** from the submenu:
   - TRIM_IF_NOT_EMPTY
   - EMPTY_OR_WHITESPACE
   - EMPTY_OR_NULL

### **Best For:**
- Quick access to smart null patterns
- Keyboard-free operation
- Context-sensitive field handling

## Method 4: Smart Null Value Suggestions

### **Step-by-Step Process:**
1. **Right-click on any field row**
2. **Select "Smart Null Value Suggestions"**
3. **Review** the suggestions for that field type

### **Best For:**
- Getting intelligent suggestions based on field type
- When you're unsure what pattern to use
- Learning about appropriate patterns for different field types

## Available Smart Patterns for String Fields

### **1. TRIM_IF_NOT_EMPTY** ⭐ (Most Useful)
- **Purpose**: Trims whitespace and treats empty results as NULL
- **Generated SQL**: `"TRIM(:field_name)" NULLIF field_name=BLANKS`
- **Use Case**: When you want to trim data but preserve non-empty values
- **Perfect for**: Names, addresses, descriptions

### **2. EMPTY_OR_WHITESPACE**
- **Purpose**: Treats empty strings and whitespace-only values as NULL
- **Generated SQL**: `NULLIF field_name=BLANKS`
- **Use Case**: When you want to convert empty or whitespace-only fields to NULL
- **Perfect for**: Optional fields, comments, notes

### **3. EMPTY_OR_NULL**
- **Purpose**: Treats empty strings and 'NULL' values as NULL
- **Generated SQL**: `NULLIF field_name=BLANKS NULLIF field_name='NULL'`
- **Use Case**: When your data contains both empty strings and literal 'NULL' values
- **Perfect for**: Legacy data with mixed null representations

## Field Type Restrictions

### **String Fields Only:**
- **VARCHAR2** - Variable-length character strings
- **CHAR** - Fixed-length character strings  
- **CLOB** - Large character objects

### **Non-String Fields:**
- Numeric fields (NUMBER, INTEGER, FLOAT) - Use specific values like `0`, `-1`
- Date fields (DATE, TIMESTAMP) - Use specific dates like `'1900-01-01'`
- Other types - Use appropriate sentinel values

## Practical Examples

### **Example 1: Customer Data Import**
```
customer_id → 0 (numeric field - use specific value)
customer_name → TRIM_IF_NOT_EMPTY (string field - trim and handle empty)
email → EMPTY_OR_WHITESPACE (string field - handle empty/whitespace)
phone → EMPTY_OR_NULL (string field - handle empty and 'NULL' values)
registration_date → '1900-01-01' (date field - use specific date)
```

### **Example 2: Employee Data Import**
```
employee_id → 0 (numeric field)
first_name → TRIM_IF_NOT_EMPTY (string field - trim names)
last_name → TRIM_IF_NOT_EMPTY (string field - trim names)
salary → 0 (numeric field)
hire_date → '1900-01-01' (date field)
department → EMPTY_OR_WHITESPACE (string field - handle empty)
```

### **Example 3: Product Data Import**
```
product_id → 0 (numeric field)
product_name → TRIM_IF_NOT_EMPTY (string field - trim product names)
description → EMPTY_OR_WHITESPACE (string field - handle empty descriptions)
price → 0 (numeric field)
category → EMPTY_OR_WHITESPACE (string field - handle empty categories)
```

## Best Practices for String Field Selection

### **1. Choose the Right Pattern for Each Field**
- **TRIM_IF_NOT_EMPTY**: For names, titles, important text that should be trimmed
- **EMPTY_OR_WHITESPACE**: For optional fields, comments, notes
- **EMPTY_OR_NULL**: For legacy data with mixed null representations

### **2. Consider Data Quality**
- Review your data to understand null value patterns
- Use the data preview feature to see actual values
- Test with sample data before production

### **3. Be Consistent Within Field Types**
- Use similar patterns for similar types of data
- Document your null value strategy
- Keep track of patterns used for each field

### **4. Validate Your Choices**
- Use the "Validate" button to check all fields
- Preview the generated control file
- Test with sample data

## Troubleshooting

### **Common Issues:**

1. **"No String Field Selected"**
   - Make sure you've selected a field with SQL type VARCHAR2, CHAR, or CLOB
   - Check the SQL Type column in the DataGridView

2. **"No Field Selected"**
   - Make sure you've selected at least one field row
   - Click on the field row before using the smart null buttons

3. **Pattern not applied**
   - Ensure you pressed Enter after typing in the Null If Value column
   - Check that the pattern name is correct (case-insensitive)

### **Tips:**
- Use the tooltips to understand what each pattern does
- Preview the control file to see the generated SQL
- Test with small data samples first
- Focus on string fields for smart null patterns

## Summary

The application now provides **field-specific smart null value selection** with these key features:

1. **Individual field editing** - Click and type in the Null If Value column
2. **Smart null buttons** - Quick buttons for string field patterns
3. **Context menu** - Right-click options for string field patterns
4. **Smart suggestions** - Intelligent recommendations per field type
5. **String field focus** - Optimized for VARCHAR2, CHAR, CLOB fields

This gives users precise control over null value handling for each individual string field, with your requested `TRIM_IF_NOT_EMPTY` feature prominently available for string fields! 