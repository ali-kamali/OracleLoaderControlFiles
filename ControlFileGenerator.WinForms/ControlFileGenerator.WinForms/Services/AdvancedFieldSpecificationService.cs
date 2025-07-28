using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Service for handling advanced field specifications in Oracle SQL*Loader control files
    /// </summary>
    public class AdvancedFieldSpecificationService
    {
        /// <summary>
        /// Validates advanced field specifications
        /// </summary>
        public List<string> ValidateAdvancedSpecifications(FieldDefinition field)
        {
            var errors = new List<string>();

            // Validate field terminator
            if (!string.IsNullOrEmpty(field.FieldTerminator))
            {
                if (!IsValidTerminatorType(field.TerminatorType))
                {
                    errors.Add($"Invalid terminator type: {field.TerminatorType}");
                }
            }

            // Validate data type modifier
            if (!string.IsNullOrEmpty(field.DataTypeModifier))
            {
                if (!IsValidDataTypeModifier(field.DataTypeModifier))
                {
                    errors.Add($"Invalid data type modifier: {field.DataTypeModifier}");
                }
            }

            // Validate precision and scale
            if (field.Precision.HasValue)
            {
                if (field.Precision.Value < 1 || field.Precision.Value > 38)
                {
                    errors.Add("Precision must be between 1 and 38");
                }
            }

            if (field.Scale.HasValue)
            {
                if (field.Scale.Value < 0 || field.Scale.Value > 127)
                {
                    errors.Add("Scale must be between 0 and 127");
                }

                if (field.Precision.HasValue && field.Scale.Value > field.Precision.Value)
                {
                    errors.Add("Scale cannot be greater than precision");
                }
            }

            // Validate LOB size
            if (field.LobSize.HasValue)
            {
                if (field.LobSize.Value < 1 || field.LobSize.Value > 4294967295)
                {
                    errors.Add("LOB size must be between 1 and 4294967295");
                }
            }

            // Validate null operator
            if (!string.IsNullOrEmpty(field.NullOperator))
            {
                if (!IsValidNullOperator(field.NullOperator))
                {
                    errors.Add($"Invalid null operator: {field.NullOperator}");
                }
            }

            // Validate transform type
            if (!string.IsNullOrEmpty(field.TransformType))
            {
                if (!IsValidTransformType(field.TransformType))
                {
                    errors.Add($"Invalid transform type: {field.TransformType}");
                }
            }

            // Validate field character set
            if (!string.IsNullOrEmpty(field.FieldCharacterSet))
            {
                if (!IsValidCharacterSet(field.FieldCharacterSet))
                {
                    errors.Add($"Invalid character set: {field.FieldCharacterSet}");
                }
            }

            // Validate position type
            if (!string.IsNullOrEmpty(field.PositionType))
            {
                if (!IsValidPositionType(field.PositionType))
                {
                    errors.Add($"Invalid position type: {field.PositionType}");
                }
            }

            return errors;
        }

        /// <summary>
        /// Generates advanced field specification examples
        /// </summary>
        public List<FieldDefinition> GenerateAdvancedExamples()
        {
            return new List<FieldDefinition>
            {
                // Example 1: Field with custom terminator
                new FieldDefinition
                {
                    FieldName = "custom_field",
                    FieldTerminator = "|",
                    TerminatorType = "CHARACTER",
                    SqlType = "VARCHAR2",
                    Length = 50
                },

                // Example 2: Field with advanced NULL handling
                new FieldDefinition
                {
                    FieldName = "status_field",
                    NullCondition = "status_field = '999' OR status_field = '000'",
                    NullOperator = "OR",
                    SqlType = "CHAR",
                    Length = 3
                },

                // Example 3: Field with CASE transformation
                new FieldDefinition
                {
                    FieldName = "status_code",
                    TransformType = "CASE",
                    TransformParameters = "WHEN A THEN ACTIVE,WHEN I THEN INACTIVE,ELSE UNKNOWN",
                    SqlType = "VARCHAR2",
                    Length = 10
                },

                // Example 4: Field with DECODE transformation
                new FieldDefinition
                {
                    FieldName = "gender",
                    TransformType = "DECODE",
                    TransformParameters = "M,MALE,F,FEMALE,UNKNOWN",
                    SqlType = "VARCHAR2",
                    Length = 6
                },

                // Example 5: Field with validation rule
                new FieldDefinition
                {
                    FieldName = "age",
                    ValidationRule = "age >= 0 AND age <= 150",
                    SqlType = "NUMBER",
                    Precision = 3
                },

                // Example 6: Field with field-level enclosure
                new FieldDefinition
                {
                    FieldName = "description",
                    FieldEnclosedBy = "\"",
                    FieldOptionallyEnclosed = true,
                    SqlType = "VARCHAR2",
                    Length = 200
                },

                // Example 7: Field with character set
                new FieldDefinition
                {
                    FieldName = "unicode_text",
                    FieldCharacterSet = "UTF8",
                    SqlType = "VARCHAR2",
                    Length = 100
                },

                // Example 8: Field with relative positioning
                new FieldDefinition
                {
                    FieldName = "relative_field",
                    RelativePosition = true,
                    SqlType = "CHAR",
                    Length = 10
                },

                // Example 9: Field with LOB specification
                new FieldDefinition
                {
                    FieldName = "large_text",
                    SqlType = "CLOB",
                    LobSize = 1000,
                    FieldTerminator = "EOF",
                    TerminatorType = "EOF"
                },

                // Example 10: Field with complex default condition
                new FieldDefinition
                {
                    FieldName = "priority",
                    DefaultCondition = "priority = BLANKS OR priority = '0'",
                    DefaultValue = "NORMAL",
                    SqlType = "VARCHAR2",
                    Length = 10
                }
            };
        }

        /// <summary>
        /// Validates terminator type
        /// </summary>
        private bool IsValidTerminatorType(string terminatorType)
        {
            var validTypes = new[] { "CHARACTER", "WHITESPACE", "EOF" };
            return validTypes.Contains(terminatorType.ToUpper());
        }

        /// <summary>
        /// Validates data type modifier
        /// </summary>
        private bool IsValidDataTypeModifier(string modifier)
        {
            var validModifiers = new[] { "EXTERNAL", "INTERNAL", "BINARY_INTEGER", "BINARY_FLOAT", "BINARY_DOUBLE" };
            return validModifiers.Contains(modifier.ToUpper());
        }

        /// <summary>
        /// Validates null operator
        /// </summary>
        private bool IsValidNullOperator(string nullOperator)
        {
            var validOperators = new[] { "AND", "OR" };
            return validOperators.Contains(nullOperator.ToUpper());
        }

        /// <summary>
        /// Validates transform type
        /// </summary>
        private bool IsValidTransformType(string transformType)
        {
            var validTypes = new[] { "CASE", "DECODE", "FUNCTION" };
            return validTypes.Contains(transformType.ToUpper());
        }

        /// <summary>
        /// Validates character set
        /// </summary>
        private bool IsValidCharacterSet(string characterSet)
        {
            var validCharacterSets = new[] { "UTF8", "AL32UTF8", "ZHS16GBK", "AL16UTF16", "WE8ISO8859P1" };
            return validCharacterSets.Contains(characterSet.ToUpper());
        }

        /// <summary>
        /// Validates position type
        /// </summary>
        private bool IsValidPositionType(string positionType)
        {
            var validTypes = new[] { "ABSOLUTE", "RELATIVE", "OVERLAPPING" };
            return validTypes.Contains(positionType.ToUpper());
        }

        /// <summary>
        /// Generates documentation for advanced field specifications
        /// </summary>
        public string GenerateDocumentation()
        {
            return @"
# Advanced Field Specifications

## Field-Level Terminators
- **CHARACTER**: TERMINATED BY 'character'
- **WHITESPACE**: TERMINATED BY WHITESPACE
- **EOF**: TERMINATED BY EOF

## Field-Level Enclosures
- **ENCLOSED BY**: Must be enclosed by specified character
- **OPTIONALLY ENCLOSED BY**: May be enclosed by specified character

## Advanced NULL Handling
- **NULLIF**: NULLIF field = value
- **Compound NULLIF**: NULLIF field1 = value1 AND field2 = value2
- **DEFAULTIF**: DEFAULTIF field = value 'default_value'

## Advanced Transformations
- **CASE**: CASE WHEN condition THEN result ELSE default END
- **DECODE**: DECODE(field, value1, result1, value2, result2, default)
- **FUNCTION**: Direct SQL function calls

## Data Type Modifiers
- **EXTERNAL**: External format (default for numeric)
- **INTERNAL**: Internal format
- **BINARY_INTEGER**: Binary integer format
- **BINARY_FLOAT**: Binary float format
- **BINARY_DOUBLE**: Binary double format

## Field Validation
- **CHECK**: CHECK (condition)
- **Validation Expression**: Custom validation logic

## Character Sets
- **UTF8**: UTF-8 encoding
- **AL32UTF8**: Oracle UTF-8 encoding
- **ZHS16GBK**: Chinese GBK encoding
- **AL16UTF16**: UTF-16 encoding
- **WE8ISO8859P1**: Western European encoding

## Position Types
- **ABSOLUTE**: Fixed position (default)
- **RELATIVE**: Relative to previous field
- **OVERLAPPING**: Overlapping field positions
";
        }
    }
} 