using ControlFileGenerator.WinForms.Models;
using System.Text.RegularExpressions;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Provides enhanced error messages with actionable suggestions based on field data and context
    /// </summary>
    public static class EnhancedErrorMessageService
    {
        /// <summary>
        /// Enhanced validation result with detailed error messages and suggestions
        /// </summary>
        public class EnhancedValidationResult
        {
            public List<EnhancedValidationError> Errors { get; set; } = new List<EnhancedValidationError>();
            public List<EnhancedValidationWarning> Warnings { get; set; } = new List<EnhancedValidationWarning>();
            public List<EnhancedValidationSuggestion> Suggestions { get; set; } = new List<EnhancedValidationSuggestion>();
            
            public bool HasErrors => Errors.Any();
            public bool HasWarnings => Warnings.Any();
            public bool HasSuggestions => Suggestions.Any();
            public bool IsValid => !HasErrors;
            
            public int ErrorCount => Errors.Count;
            public int WarningCount => Warnings.Count;
            public int SuggestionCount => Suggestions.Count;
        }

        public class EnhancedValidationError
        {
            public string FieldName { get; set; } = string.Empty;
            public string Property { get; set; } = string.Empty;
            public string ErrorMessage { get; set; } = string.Empty;
            public string Suggestion { get; set; } = string.Empty;
            public string AutoFixAction { get; set; } = string.Empty;
            public ValidationSeverity Severity { get; set; } = ValidationSeverity.Error;
        }

        public class EnhancedValidationWarning
        {
            public string FieldName { get; set; } = string.Empty;
            public string Property { get; set; } = string.Empty;
            public string WarningMessage { get; set; } = string.Empty;
            public string Suggestion { get; set; } = string.Empty;
            public string AutoFixAction { get; set; } = string.Empty;
            public ValidationSeverity Severity { get; set; } = ValidationSeverity.Warning;
        }

        public class EnhancedValidationSuggestion
        {
            public string FieldName { get; set; } = string.Empty;
            public string Property { get; set; } = string.Empty;
            public string SuggestionMessage { get; set; } = string.Empty;
            public string Implementation { get; set; } = string.Empty;
            public ValidationSeverity Severity { get; set; } = ValidationSeverity.Suggestion;
        }

        public enum ValidationSeverity
        {
            Error,
            Warning,
            Suggestion,
            Info
        }

        /// <summary>
        /// Validates a field definition with enhanced error messages and suggestions
        /// </summary>
        public static EnhancedValidationResult ValidateFieldWithSuggestions(FieldDefinition field, List<FieldDefinition> allFields)
        {
            var result = new EnhancedValidationResult();
            
            // Validate field name
            ValidateFieldName(field, result);
            
            // Validate field type and mapping
            ValidateFieldType(field, result);
            
            // Validate positions and lengths
            ValidatePositionsAndLengths(field, allFields, result);
            
            // Validate data consistency
            ValidateDataConsistency(field, result);
            
            // Validate advanced properties
            ValidateAdvancedProperties(field, result);
            
            // Generate optimization suggestions
            GenerateOptimizationSuggestions(field, allFields, result);
            
            return result;
        }

        /// <summary>
        /// Validates all fields with enhanced error messages and suggestions
        /// </summary>
        public static EnhancedValidationResult ValidateAllFieldsWithSuggestions(List<FieldDefinition> fields)
        {
            var result = new EnhancedValidationResult();
            
            foreach (var field in fields)
            {
                var fieldResult = ValidateFieldWithSuggestions(field, fields);
                result.Errors.AddRange(fieldResult.Errors);
                result.Warnings.AddRange(fieldResult.Warnings);
                result.Suggestions.AddRange(fieldResult.Suggestions);
            }
            
            // Validate cross-field relationships
            ValidateCrossFieldRelationships(fields, result);
            
            return result;
        }

        #region Private Validation Methods

        private static void ValidateFieldName(FieldDefinition field, EnhancedValidationResult result)
        {
            if (string.IsNullOrWhiteSpace(field.FieldName))
            {
                result.Errors.Add(new EnhancedValidationError
                {
                    FieldName = "Unknown",
                    Property = "FieldName",
                    ErrorMessage = "Field name is required and cannot be empty.",
                    Suggestion = "Enter a descriptive field name that follows Oracle naming conventions.",
                    AutoFixAction = "Cannot auto-fix - requires user input"
                });
                return;
            }

            if (field.FieldName.Length > 30)
            {
                result.Errors.Add(new EnhancedValidationError
                {
                    FieldName = field.FieldName,
                    Property = "FieldName",
                    ErrorMessage = $"Field name '{field.FieldName}' exceeds 30 characters (current length: {field.FieldName.Length}).",
                    Suggestion = "Shorten the field name to 30 characters or less. Consider using abbreviations while maintaining clarity.",
                    AutoFixAction = $"Truncate to: {field.FieldName.Substring(0, 30)}"
                });
            }

            if (!Regex.IsMatch(field.FieldName, @"^[A-Za-z_][A-Za-z0-9_]*$"))
            {
                result.Errors.Add(new EnhancedValidationError
                {
                    FieldName = field.FieldName,
                    Property = "FieldName",
                    ErrorMessage = $"Field name '{field.FieldName}' contains invalid characters.",
                    Suggestion = "Field names must start with a letter or underscore and contain only letters, numbers, and underscores.",
                    AutoFixAction = $"Suggested: {Regex.Replace(field.FieldName, @"[^A-Za-z0-9_]", "_")}"
                });
            }

            // Check for reserved words
            var reservedWords = new[] { "SELECT", "FROM", "WHERE", "ORDER", "GROUP", "BY", "HAVING", "UNION", "INSERT", "UPDATE", "DELETE", "CREATE", "DROP", "ALTER", "TABLE", "INDEX", "VIEW", "SEQUENCE", "SYNONYM", "GRANT", "REVOKE", "COMMIT", "ROLLBACK", "SAVEPOINT", "TRANSACTION", "SESSION", "USER", "SYSDATE", "SYSTIMESTAMP", "NULL", "TRUE", "FALSE" };
            if (reservedWords.Contains(field.FieldName.ToUpper()))
            {
                result.Warnings.Add(new EnhancedValidationWarning
                {
                    FieldName = field.FieldName,
                    Property = "FieldName",
                    WarningMessage = $"Field name '{field.FieldName}' is a reserved Oracle keyword.",
                    Suggestion = "Consider using a different name to avoid potential conflicts. You can still use it, but it's not recommended.",
                    AutoFixAction = $"Suggested: {field.FieldName}_FIELD"
                });
            }
        }

        private static void ValidateFieldType(FieldDefinition field, EnhancedValidationResult result)
        {
            // Validate COBOL type
            if (!string.IsNullOrWhiteSpace(field.CobolType))
            {
                if (!IsValidCobolType(field.CobolType))
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "CobolType",
                        ErrorMessage = $"Invalid COBOL type format: '{field.CobolType}'",
                        Suggestion = "Use standard COBOL PIC format (e.g., 'PIC X(10)', 'PIC 9(5)', 'PIC S9(7)V99').",
                        AutoFixAction = "Cannot auto-fix - requires user input"
                    });
                }
                else
                {
                    // Suggest SQL type if not provided
                    if (string.IsNullOrWhiteSpace(field.SqlType))
                    {
                        var suggestedSqlType = CobolTypeMapper.MapCobolToOracle(field.CobolType);
                        result.Suggestions.Add(new EnhancedValidationSuggestion
                        {
                            FieldName = field.FieldName,
                            Property = "SqlType",
                            SuggestionMessage = $"COBOL type '{field.CobolType}' maps to Oracle type '{suggestedSqlType}'",
                            Implementation = $"Set SQL Type to: {suggestedSqlType}"
                        });
                    }
                }
            }

            // Validate SQL type
            if (!string.IsNullOrWhiteSpace(field.SqlType))
            {
                if (!IsValidSqlType(field.SqlType))
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "SqlType",
                        ErrorMessage = $"Invalid SQL type: '{field.SqlType}'",
                        Suggestion = "Use valid Oracle data types: CHAR, VARCHAR2, NUMBER, DATE, TIMESTAMP, CLOB, BLOB, RAW, LONG, LONG RAW.",
                        AutoFixAction = "Cannot auto-fix - requires user input"
                    });
                }
                else
                {
                    // Validate precision and scale for NUMBER types
                    if (field.SqlType.ToUpper().StartsWith("NUMBER"))
                    {
                        ValidateNumberPrecisionAndScale(field, result);
                    }
                }
            }
            else if (string.IsNullOrWhiteSpace(field.CobolType))
            {
                result.Warnings.Add(new EnhancedValidationWarning
                {
                    FieldName = field.FieldName,
                    Property = "SqlType",
                    WarningMessage = "Neither COBOL type nor SQL type is specified.",
                    Suggestion = "Specify either a COBOL type (for automatic mapping) or an Oracle SQL type.",
                    AutoFixAction = "Set default SQL Type to: CHAR(255)"
                });
            }
        }

        private static void ValidatePositionsAndLengths(FieldDefinition field, List<FieldDefinition> allFields, EnhancedValidationResult result)
        {
            if (field.IsVirtual)
                return; // Virtual fields don't need positions

            // Check for missing position information
            if (!field.StartPosition.HasValue && !field.EndPosition.HasValue && !field.Length.HasValue && !field.Order.HasValue)
            {
                result.Errors.Add(new EnhancedValidationError
                {
                    FieldName = field.FieldName,
                    Property = "Position",
                    ErrorMessage = "No position information specified for non-virtual field.",
                    Suggestion = "Specify either start/end positions, length, or order for automatic position calculation.",
                    AutoFixAction = "Set Order to next available number and calculate positions automatically"
                });
            }

            // Validate position consistency
            if (field.StartPosition.HasValue && field.EndPosition.HasValue)
            {
                if (field.StartPosition.Value > field.EndPosition.Value)
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "Position",
                        ErrorMessage = $"Start position ({field.StartPosition.Value}) cannot be greater than end position ({field.EndPosition.Value}).",
                        Suggestion = "Swap start and end positions or adjust the values.",
                        AutoFixAction = $"Swap positions: Start={field.EndPosition.Value}, End={field.StartPosition.Value}"
                    });
                }

                // Check for overlaps with other fields
                var overlappingFields = allFields.Where(f => 
                    !f.IsVirtual && 
                    f != field && 
                    f.StartPosition.HasValue && 
                    f.EndPosition.HasValue &&
                    !(field.EndPosition.Value < f.StartPosition.Value || field.StartPosition.Value > f.EndPosition.Value)
                ).ToList();

                foreach (var overlappingField in overlappingFields)
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "Position",
                        ErrorMessage = $"Position range ({field.StartPosition.Value}-{field.EndPosition.Value}) overlaps with field '{overlappingField.FieldName}' ({overlappingField.StartPosition.Value}-{overlappingField.EndPosition.Value}).",
                        Suggestion = "Adjust the position range to avoid overlap with other fields.",
                        AutoFixAction = $"Calculate next available position after field '{overlappingField.FieldName}'"
                    });
                }
            }

            // Validate length consistency
            if (field.StartPosition.HasValue && field.EndPosition.HasValue && field.Length.HasValue)
            {
                var calculatedLength = field.EndPosition.Value - field.StartPosition.Value + 1;
                if (calculatedLength != field.Length.Value)
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "Length",
                        ErrorMessage = $"Length ({field.Length.Value}) does not match position range ({field.StartPosition.Value}-{field.EndPosition.Value}).",
                        Suggestion = "Adjust length to match the position range or vice versa.",
                        AutoFixAction = $"Set Length to: {calculatedLength}"
                    });
                }
            }

            // Suggest length based on COBOL type
            if (!field.Length.HasValue && !string.IsNullOrWhiteSpace(field.CobolType))
            {
                var suggestedLength = field.CalculateLengthFromCobolType(field.CobolType);
                if (suggestedLength > 0)
                {
                    result.Suggestions.Add(new EnhancedValidationSuggestion
                    {
                        FieldName = field.FieldName,
                        Property = "Length",
                        SuggestionMessage = $"COBOL type '{field.CobolType}' suggests length of {suggestedLength}",
                        Implementation = $"Set Length to: {suggestedLength}"
                    });
                }
            }
        }

        private static void ValidateDataConsistency(FieldDefinition field, EnhancedValidationResult result)
        {
            // Validate nullable value
            if (!string.IsNullOrEmpty(field.Nullable))
            {
                var normalizedNullable = field.Nullable.ToUpper();
                if (normalizedNullable != "YES" && normalizedNullable != "NO")
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "Nullable",
                        ErrorMessage = $"Invalid nullable value: '{field.Nullable}'",
                        Suggestion = "Use 'YES' or 'NO' for nullable specification.",
                        AutoFixAction = "Set Nullable to: YES"
                    });
                }
            }

            // Validate null if value
            if (!string.IsNullOrEmpty(field.NullIfValue))
            {
                var suggestions = IntelligentNullValueProcessor.GetSuggestedNullPatterns(field.SqlType, field.CobolType);
                if (!suggestions.Contains(field.NullIfValue) && !IsValidNullValue(field.NullIfValue, field.SqlType))
                {
                    result.Warnings.Add(new EnhancedValidationWarning
                    {
                        FieldName = field.FieldName,
                        Property = "NullIfValue",
                        WarningMessage = $"Null if value '{field.NullIfValue}' may not be appropriate for type '{field.SqlType}'",
                        Suggestion = $"Consider using one of these patterns: {string.Join(", ", suggestions.Take(3))}",
                        AutoFixAction = "Cannot auto-fix - requires user selection"
                    });
                }
            }

            // Validate default value
            if (!string.IsNullOrEmpty(field.DefaultValue))
            {
                if (!IsValidDefaultValue(field.DefaultValue, field.SqlType))
                {
                    result.Warnings.Add(new EnhancedValidationWarning
                    {
                        FieldName = field.FieldName,
                        Property = "DefaultValue",
                        WarningMessage = $"Default value '{field.DefaultValue}' may not be compatible with type '{field.SqlType}'",
                        Suggestion = "Ensure the default value matches the data type format.",
                        AutoFixAction = "Cannot auto-fix - requires user input"
                    });
                }
            }
        }

        private static void ValidateAdvancedProperties(FieldDefinition field, EnhancedValidationResult result)
        {
            // Validate precision and scale
            if (field.Precision.HasValue)
            {
                if (field.Precision.Value < 1 || field.Precision.Value > 38)
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "Precision",
                        ErrorMessage = $"Precision {field.Precision.Value} is out of valid range (1-38).",
                        Suggestion = "Oracle NUMBER precision must be between 1 and 38.",
                        AutoFixAction = $"Set Precision to: {Math.Min(Math.Max(field.Precision.Value, 1), 38)}"
                    });
                }
            }

            if (field.Scale.HasValue)
            {
                if (field.Scale.Value < 0 || field.Scale.Value > 127)
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "Scale",
                        ErrorMessage = $"Scale {field.Scale.Value} is out of valid range (0-127).",
                        Suggestion = "Oracle NUMBER scale must be between 0 and 127.",
                        AutoFixAction = $"Set Scale to: {Math.Min(Math.Max(field.Scale.Value, 0), 127)}"
                    });
                }

                if (field.Precision.HasValue && field.Scale.Value > field.Precision.Value)
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "Scale",
                        ErrorMessage = $"Scale ({field.Scale.Value}) cannot be greater than precision ({field.Precision.Value}).",
                        Suggestion = "Scale must be less than or equal to precision.",
                        AutoFixAction = $"Set Scale to: {field.Precision.Value}"
                    });
                }
            }

            // Validate LOB size
            if (field.LobSize.HasValue)
            {
                if (field.LobSize.Value < 1 || field.LobSize.Value > 4294967295)
                {
                    result.Errors.Add(new EnhancedValidationError
                    {
                        FieldName = field.FieldName,
                        Property = "LobSize",
                        ErrorMessage = $"LOB size {field.LobSize.Value} is out of valid range (1-4294967295).",
                        Suggestion = "LOB size must be between 1 and 4294967295 bytes.",
                        AutoFixAction = $"Set LobSize to: {Math.Min(Math.Max(field.LobSize.Value, 1), 4294967295)}"
                    });
                }
            }
        }

        private static void ValidateCrossFieldRelationships(List<FieldDefinition> fields, EnhancedValidationResult result)
        {
            // Check for duplicate field names
            var duplicateNames = fields.GroupBy(f => f.FieldName.ToUpper())
                .Where(g => g.Count() > 1)
                .Select(g => g.Key)
                .ToList();

            foreach (var duplicateName in duplicateNames)
            {
                var duplicateFields = fields.Where(f => f.FieldName.ToUpper() == duplicateName).ToList();
                result.Errors.Add(new EnhancedValidationError
                {
                    FieldName = duplicateName,
                    Property = "FieldName",
                    ErrorMessage = $"Duplicate field name '{duplicateName}' found in {duplicateFields.Count} fields.",
                    Suggestion = "Ensure each field has a unique name. Consider adding suffixes or prefixes.",
                    AutoFixAction = "Cannot auto-fix - requires user input"
                });
            }

            // Check for gaps in position ranges
            var nonVirtualFields = fields.Where(f => !f.IsVirtual && f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            for (int i = 0; i < nonVirtualFields.Count - 1; i++)
            {
                var current = nonVirtualFields[i];
                var next = nonVirtualFields[i + 1];
                
                if (current.EndPosition.Value + 1 < next.StartPosition.Value)
                {
                    result.Warnings.Add(new EnhancedValidationWarning
                    {
                        FieldName = next.FieldName,
                        Property = "Position",
                        WarningMessage = $"Gap detected between field '{current.FieldName}' (ends at {current.EndPosition.Value}) and field '{next.FieldName}' (starts at {next.StartPosition.Value}).",
                        Suggestion = "Consider if the gap is intentional or if fields should be repositioned.",
                        AutoFixAction = $"Move field '{next.FieldName}' to start at position {current.EndPosition.Value + 1}"
                    });
                }
            }
        }

        private static void GenerateOptimizationSuggestions(FieldDefinition field, List<FieldDefinition> allFields, EnhancedValidationResult result)
        {
            // Suggest transforms based on field type
            if (string.IsNullOrEmpty(field.Transform) && !string.IsNullOrEmpty(field.SqlType))
            {
                var transformSuggestions = SmartAutoCompletionService.GetTransformSuggestions(field.SqlType, field.FieldName);
                if (transformSuggestions.Any())
                {
                    result.Suggestions.Add(new EnhancedValidationSuggestion
                    {
                        FieldName = field.FieldName,
                        Property = "Transform",
                        SuggestionMessage = $"Consider adding a transform for better data quality.",
                        Implementation = $"Suggested transforms: {string.Join(", ", transformSuggestions.Take(3))}"
                    });
                }
            }

            // Suggest null value patterns
            if (string.IsNullOrEmpty(field.NullIfValue) && !string.IsNullOrEmpty(field.SqlType))
            {
                var nullValueSuggestions = SmartAutoCompletionService.GetNullValueSuggestions(field.SqlType, field.FieldName);
                if (nullValueSuggestions.Any())
                {
                    result.Suggestions.Add(new EnhancedValidationSuggestion
                    {
                        FieldName = field.FieldName,
                        Property = "NullIfValue",
                        SuggestionMessage = $"Consider adding null value handling for better data quality.",
                        Implementation = $"Suggested patterns: {string.Join(", ", nullValueSuggestions.Take(3))}"
                    });
                }
            }

            // Suggest data format for date/timestamp fields
            if ((field.SqlType.ToUpper().StartsWith("DATE") || field.SqlType.ToUpper().StartsWith("TIMESTAMP")) && 
                string.IsNullOrEmpty(field.DataFormat))
            {
                var formatSuggestions = SmartAutoCompletionService.GetDataFormatSuggestions(field.SqlType, field.FieldName);
                if (formatSuggestions.Any())
                {
                    result.Suggestions.Add(new EnhancedValidationSuggestion
                    {
                        FieldName = field.FieldName,
                        Property = "DataFormat",
                        SuggestionMessage = $"Consider specifying a data format for consistent date/timestamp handling.",
                        Implementation = $"Suggested formats: {string.Join(", ", formatSuggestions.Take(3))}"
                    });
                }
            }
        }

        #endregion

        #region Helper Methods

        private static bool IsValidCobolType(string cobolType)
        {
            if (string.IsNullOrWhiteSpace(cobolType))
                return false;

            var patterns = new[]
            {
                @"^PIC\s+X\(\d+\)$",
                @"^PIC\s+X$",
                @"^PIC\s+9\(\d+\)$",
                @"^PIC\s+9$",
                @"^PIC\s+S9\(\d+\)$",
                @"^PIC\s+S9$",
                @"^PIC\s+[S]?9\(\d+\)V\d+$",
                @"^PIC\s+[S]?9V\d+$",
                @"^PIC\s+9\(8\)$",
                @"^PIC\s+9\(6\)$",
                @"^PIC\s+9\(7\)$"
            };

            return patterns.Any(pattern => Regex.IsMatch(cobolType.Trim().ToUpper(), pattern));
        }

        private static bool IsValidSqlType(string sqlType)
        {
            if (string.IsNullOrWhiteSpace(sqlType))
                return false;

            var validTypes = new[]
            {
                "CHAR", "VARCHAR2", "NUMBER", "DATE", "TIMESTAMP", "CLOB", "BLOB", "RAW", "LONG", "LONG RAW",
                "NCHAR", "NVARCHAR2", "NCLOB", "BFILE", "BINARY_FLOAT", "BINARY_DOUBLE", "INTERVAL YEAR TO MONTH",
                "INTERVAL DAY TO SECOND", "TIMESTAMP WITH TIME ZONE", "TIMESTAMP WITH LOCAL TIME ZONE"
            };

            var baseType = GetBaseType(sqlType.ToUpper());
            return validTypes.Any(t => baseType.StartsWith(t));
        }

        private static string GetBaseType(string sqlType)
        {
            if (sqlType.StartsWith("CHAR"))
                return "CHAR";
            if (sqlType.StartsWith("VARCHAR"))
                return "VARCHAR2";
            if (sqlType.StartsWith("NUMBER"))
                return "NUMBER";
            if (sqlType.StartsWith("DATE"))
                return "DATE";
            if (sqlType.StartsWith("TIMESTAMP"))
                return "TIMESTAMP";
            if (sqlType.StartsWith("CLOB"))
                return "CLOB";
            if (sqlType.StartsWith("BLOB"))
                return "BLOB";
            if (sqlType.StartsWith("RAW"))
                return "RAW";
            if (sqlType.StartsWith("LONG"))
                return "LONG";
            
            return sqlType;
        }

        private static void ValidateNumberPrecisionAndScale(FieldDefinition field, EnhancedValidationResult result)
        {
            var sqlType = field.SqlType.ToUpper();
            var match = Regex.Match(sqlType, @"NUMBER\((\d+)(?:,(\d+))?\)");
            
            if (match.Success)
            {
                if (int.TryParse(match.Groups[1].Value, out int precision))
                {
                    if (precision < 1 || precision > 38)
                    {
                        result.Errors.Add(new EnhancedValidationError
                        {
                            FieldName = field.FieldName,
                            Property = "SqlType",
                            ErrorMessage = $"NUMBER precision {precision} is out of valid range (1-38).",
                            Suggestion = "Oracle NUMBER precision must be between 1 and 38.",
                            AutoFixAction = $"Use NUMBER({Math.Min(Math.Max(precision, 1), 38)})"
                        });
                    }
                }

                if (match.Groups[2].Success && int.TryParse(match.Groups[2].Value, out int scale))
                {
                    if (scale < 0 || scale > 127)
                    {
                        result.Errors.Add(new EnhancedValidationError
                        {
                            FieldName = field.FieldName,
                            Property = "SqlType",
                            ErrorMessage = $"NUMBER scale {scale} is out of valid range (0-127).",
                            Suggestion = "Oracle NUMBER scale must be between 0 and 127.",
                            AutoFixAction = $"Use NUMBER({precision},{Math.Min(Math.Max(scale, 0), 127)})"
                        });
                    }
                }
            }
        }

        private static bool IsValidNullValue(string nullValue, string sqlType)
        {
            if (string.IsNullOrWhiteSpace(nullValue))
                return false;

            // Check for smart patterns
            var smartPatterns = new[]
            {
                IntelligentNullValueProcessor.SmartNullPatterns.EMPTY_OR_WHITESPACE,
                IntelligentNullValueProcessor.SmartNullPatterns.TRIM_IF_NOT_EMPTY,
                IntelligentNullValueProcessor.SmartNullPatterns.EMPTY_OR_NULL,
                IntelligentNullValueProcessor.SmartNullPatterns.BLANKS_OR_EMPTY
            };

            if (smartPatterns.Contains(nullValue.ToUpper()))
                return true;

            // Check for quoted strings
            if ((nullValue.StartsWith("'") && nullValue.EndsWith("'")) || 
                (nullValue.StartsWith("\"") && nullValue.EndsWith("\"")))
                return true;

            // Check for numeric values
            if (decimal.TryParse(nullValue, out _))
                return true;

            // Check for special values
            var specialValues = new[] { "BLANKS", "NULL", "ZERO", "SPACE" };
            return specialValues.Contains(nullValue.ToUpper());
        }

        private static bool IsValidDefaultValue(string defaultValue, string sqlType)
        {
            if (string.IsNullOrWhiteSpace(defaultValue))
                return false;

            // Check for Oracle functions
            var oracleFunctions = new[] { "SYSDATE", "SYSTIMESTAMP", "USER", "UID", "SYSDATE - 1", "TRUNC(SYSDATE)" };
            if (oracleFunctions.Contains(defaultValue.ToUpper()))
                return true;

            // Check for quoted strings
            if ((defaultValue.StartsWith("'") && defaultValue.EndsWith("'")) || 
                (defaultValue.StartsWith("\"") && defaultValue.EndsWith("\"")))
                return true;

            // Check for numeric values
            if (decimal.TryParse(defaultValue, out _))
                return true;

            return true; // Assume valid if no specific validation fails
        }

        #endregion
    }
} 