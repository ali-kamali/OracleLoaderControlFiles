using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Provides intelligent processing of null values for Oracle SQL*Loader control files
    /// </summary>
    public static class IntelligentNullValueProcessor
    {
        /// <summary>
        /// Smart null value patterns that handle common empty data scenarios
        /// </summary>
        public static class SmartNullPatterns
        {
            public const string EMPTY_OR_WHITESPACE = "EMPTY_OR_WHITESPACE";
            public const string TRIM_IF_NOT_EMPTY = "TRIM_IF_NOT_EMPTY";
            public const string EMPTY_OR_NULL = "EMPTY_OR_NULL";
            public const string BLANKS_OR_EMPTY = "BLANKS_OR_EMPTY";
        }

        /// <summary>
        /// Processes a null value pattern and returns the appropriate Oracle SQL*Loader clause
        /// </summary>
        public static string ProcessNullValuePattern(string nullIfValue, string fieldName)
        {
            if (string.IsNullOrWhiteSpace(nullIfValue))
                return string.Empty;

            // Handle smart patterns
            switch (nullIfValue.ToUpper())
            {
                case SmartNullPatterns.EMPTY_OR_WHITESPACE:
                    return GenerateEmptyOrWhitespaceClause(fieldName);
                
                case SmartNullPatterns.TRIM_IF_NOT_EMPTY:
                    return GenerateTrimIfNotEmptyClause(fieldName);
                
                case SmartNullPatterns.EMPTY_OR_NULL:
                    return GenerateEmptyOrNullClause(fieldName);
                
                case SmartNullPatterns.BLANKS_OR_EMPTY:
                    return GenerateBlanksOrEmptyClause(fieldName);
                
                default:
                    // Handle as regular null value
                    return GenerateStandardNullIfClause(fieldName, nullIfValue);
            }
        }

        /// <summary>
        /// Generates clause for empty or whitespace-only values
        /// </summary>
        private static string GenerateEmptyOrWhitespaceClause(string fieldName)
        {
            return $"NULLIF {fieldName}=BLANKS";
        }

        /// <summary>
        /// Generates clause that trims data if not empty, then checks for null
        /// </summary>
        private static string GenerateTrimIfNotEmptyClause(string fieldName)
        {
            // This requires a transform to trim the data first
            return $"\"TRIM(:{fieldName})\" NULLIF {fieldName}=BLANKS";
        }

        /// <summary>
        /// Generates clause for empty strings or NULL values
        /// </summary>
        private static string GenerateEmptyOrNullClause(string fieldName)
        {
            return $"NULLIF {fieldName}=BLANKS NULLIF {fieldName}='NULL'";
        }

        /// <summary>
        /// Generates clause for blanks or empty strings
        /// </summary>
        private static string GenerateBlanksOrEmptyClause(string fieldName)
        {
            return $"NULLIF {fieldName}=BLANKS NULLIF {fieldName}=''";
        }

        /// <summary>
        /// Generates standard NULLIF clause
        /// </summary>
        private static string GenerateStandardNullIfClause(string fieldName, string nullValue)
        {
            // Handle quoted vs unquoted values
            if (nullValue.StartsWith("'") && nullValue.EndsWith("'"))
            {
                return $"NULLIF {fieldName}={nullValue}";
            }
            else if (IsNumericValue(nullValue))
            {
                return $"NULLIF {fieldName}={nullValue}";
            }
            else
            {
                // Auto-quote string values
                return $"NULLIF {fieldName}='{nullValue}'";
            }
        }

        /// <summary>
        /// Checks if a value is numeric
        /// </summary>
        private static bool IsNumericValue(string value)
        {
            return decimal.TryParse(value, out _);
        }

        /// <summary>
        /// Gets suggested null value patterns based on field type
        /// </summary>
        public static List<string> GetSuggestedNullPatterns(string sqlType, string cobolType)
        {
            var suggestions = new List<string>();

            // Add smart patterns
            suggestions.Add(SmartNullPatterns.EMPTY_OR_WHITESPACE);
            suggestions.Add(SmartNullPatterns.TRIM_IF_NOT_EMPTY);

            // Add type-specific suggestions
            if (IsNumericType(sqlType))
            {
                suggestions.AddRange(new[] { "0", "-1", "-999", "999999" });
            }
            else if (IsDateType(sqlType))
            {
                suggestions.AddRange(new[] { "'1900-01-01'", "'0000-00-00'", "'00:00:00'" });
            }
            else
            {
                suggestions.AddRange(new[] { "''", "' '", "'NULL'", "'N/A'", "'UNKNOWN'" });
            }

            return suggestions;
        }

        /// <summary>
        /// Checks if SQL type is numeric
        /// </summary>
        private static bool IsNumericType(string sqlType)
        {
            if (string.IsNullOrWhiteSpace(sqlType))
                return false;

            var numericTypes = new[] { "NUMBER", "INTEGER", "FLOAT", "DECIMAL", "NUMERIC", "BINARY_FLOAT", "BINARY_DOUBLE" };
            return numericTypes.Any(t => sqlType.ToUpper().StartsWith(t));
        }

        /// <summary>
        /// Checks if SQL type is date/time
        /// </summary>
        private static bool IsDateType(string sqlType)
        {
            if (string.IsNullOrWhiteSpace(sqlType))
                return false;

            var dateTypes = new[] { "DATE", "TIMESTAMP", "INTERVAL" };
            return dateTypes.Any(t => sqlType.ToUpper().StartsWith(t));
        }

        /// <summary>
        /// Validates a null value pattern
        /// </summary>
        public static List<string> ValidateNullValuePattern(string nullIfValue)
        {
            var errors = new List<string>();

            if (string.IsNullOrWhiteSpace(nullIfValue))
                return errors;

            // Check if it's a smart pattern
            var smartPatterns = new[] 
            { 
                SmartNullPatterns.EMPTY_OR_WHITESPACE,
                SmartNullPatterns.TRIM_IF_NOT_EMPTY,
                SmartNullPatterns.EMPTY_OR_NULL,
                SmartNullPatterns.BLANKS_OR_EMPTY
            };

            if (smartPatterns.Contains(nullIfValue.ToUpper()))
                return errors;

            // Validate quoted strings
            if (nullIfValue.StartsWith("'") && !nullIfValue.EndsWith("'"))
            {
                errors.Add("Unclosed quote in null value");
            }

            // Validate numeric values
            if (!nullIfValue.StartsWith("'") && !IsNumericValue(nullIfValue))
            {
                errors.Add("Non-numeric value should be quoted");
            }

            return errors;
        }

        /// <summary>
        /// Gets description for a null value pattern
        /// </summary>
        public static string GetNullValueDescription(string nullIfValue)
        {
            if (string.IsNullOrWhiteSpace(nullIfValue))
                return "No null value specified";

            switch (nullIfValue.ToUpper())
            {
                case SmartNullPatterns.EMPTY_OR_WHITESPACE:
                    return "Treats empty strings and whitespace-only values as NULL";
                
                case SmartNullPatterns.TRIM_IF_NOT_EMPTY:
                    return "Trims whitespace and treats empty results as NULL";
                
                case SmartNullPatterns.EMPTY_OR_NULL:
                    return "Treats empty strings and 'NULL' values as NULL";
                
                case SmartNullPatterns.BLANKS_OR_EMPTY:
                    return "Treats Oracle BLANKS and empty strings as NULL";
                
                default:
                    return $"Treats '{nullIfValue}' as NULL";
            }
        }

        /// <summary>
        /// Gets smart null value suggestions based on field type and characteristics
        /// </summary>
        public static List<string> GetSmartNullValueSuggestions(FieldDefinition field)
        {
            var suggestions = new List<string>();

            // Always include smart patterns
            suggestions.Add(SmartNullPatterns.EMPTY_OR_WHITESPACE);
            suggestions.Add(SmartNullPatterns.TRIM_IF_NOT_EMPTY);

            // Add type-specific suggestions
            if (IsNumericType(field.SqlType))
            {
                suggestions.AddRange(new[] { "0", "-1", "-999", "999999" });
            }
            else if (IsDateType(field.SqlType))
            {
                suggestions.AddRange(new[] { "'1900-01-01'", "'0000-00-00'", "'00:00:00'" });
            }
            else
            {
                suggestions.AddRange(new[] { "''", "' '", "'NULL'", "'N/A'", "'UNKNOWN'" });
            }

            // Add COBOL-specific suggestions if available
            if (!string.IsNullOrEmpty(field.CobolType))
            {
                var cobolSuggestions = GetCobolSpecificSuggestions(field.CobolType);
                suggestions.AddRange(cobolSuggestions);
            }

            return suggestions.Distinct().ToList();
        }

        /// <summary>
        /// Gets COBOL-specific null value suggestions
        /// </summary>
        private static List<string> GetCobolSpecificSuggestions(string cobolType)
        {
            var suggestions = new List<string>();
            var normalizedType = cobolType.Trim().ToUpper();

            if (normalizedType.Contains("PIC 9"))
            {
                // Numeric fields
                suggestions.AddRange(new[] { "0", "999999", "999999999" });
            }
            else if (normalizedType.Contains("PIC X"))
            {
                // Character fields
                suggestions.AddRange(new[] { "''", "' '", "'BLANKS'" });
            }
            else if (normalizedType.Contains("PIC S"))
            {
                // Signed numeric fields
                suggestions.AddRange(new[] { "0", "-1", "-999" });
            }

            return suggestions;
        }
    }
} 