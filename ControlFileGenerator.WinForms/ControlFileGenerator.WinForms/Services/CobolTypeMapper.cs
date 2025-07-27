using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Services
{
    public static class CobolTypeMapper
    {
        /// <summary>
        /// COBOL to Oracle type mapping dictionary
        /// </summary>
        private static readonly Dictionary<string, string> _cobolToOracleMapping = new(StringComparer.OrdinalIgnoreCase)
        {
            // Numeric types
            { "PIC 9", "NUMBER" },
            { "PIC 9(1)", "NUMBER(1)" },
            { "PIC 9(2)", "NUMBER(2)" },
            { "PIC 9(3)", "NUMBER(3)" },
            { "PIC 9(4)", "NUMBER(4)" },
            { "PIC 9(5)", "NUMBER(5)" },
            { "PIC 9(6)", "NUMBER(6)" },
            { "PIC 9(7)", "NUMBER(7)" },
            { "PIC 9(8)", "NUMBER(8)" },
            { "PIC 9(9)", "NUMBER(9)" },
            { "PIC 9(10)", "NUMBER(10)" },
            { "PIC 9(11)", "NUMBER(11)" },
            { "PIC 9(12)", "NUMBER(12)" },
            { "PIC 9(13)", "NUMBER(13)" },
            { "PIC 9(14)", "NUMBER(14)" },
            { "PIC 9(15)", "NUMBER(15)" },
            { "PIC 9(16)", "NUMBER(16)" },
            { "PIC 9(17)", "NUMBER(17)" },
            { "PIC 9(18)", "NUMBER(18)" },
            { "PIC 9(19)", "NUMBER(19)" },
            { "PIC 9(20)", "NUMBER(20)" },
            
            // Decimal types
            { "PIC 9(5)V99", "NUMBER(7,2)" },
            { "PIC 9(6)V99", "NUMBER(8,2)" },
            { "PIC 9(7)V99", "NUMBER(9,2)" },
            { "PIC 9(8)V99", "NUMBER(10,2)" },
            { "PIC 9(9)V99", "NUMBER(11,2)" },
            { "PIC 9(10)V99", "NUMBER(12,2)" },
            { "PIC 9(5)V999", "NUMBER(8,3)" },
            { "PIC 9(6)V999", "NUMBER(9,3)" },
            { "PIC 9(7)V999", "NUMBER(10,3)" },
            { "PIC 9(8)V999", "NUMBER(11,3)" },
            { "PIC 9(9)V999", "NUMBER(12,3)" },
            { "PIC 9(10)V999", "NUMBER(13,3)" },
            
            // Character types
            { "PIC X", "VARCHAR2(1)" },
            { "PIC XX", "CHAR(2)" },
            { "PIC X(1)", "CHAR(1)" },
            { "PIC X(2)", "CHAR(2)" },
            { "PIC X(3)", "CHAR(3)" },
            { "PIC X(4)", "CHAR(4)" },
            { "PIC X(5)", "CHAR(5)" },
            { "PIC X(6)", "CHAR(6)" },
            { "PIC X(7)", "CHAR(7)" },
            { "PIC X(8)", "CHAR(8)" },
            { "PIC X(9)", "CHAR(9)" },
            { "PIC X(10)", "CHAR(10)" },
            { "PIC X(15)", "VARCHAR2(15)" },
            { "PIC X(20)", "VARCHAR2(20)" },
            { "PIC X(25)", "VARCHAR2(25)" },
            { "PIC X(30)", "VARCHAR2(30)" },
            { "PIC X(40)", "VARCHAR2(40)" },
            { "PIC X(50)", "VARCHAR2(50)" },
            { "PIC X(60)", "VARCHAR2(60)" },
            { "PIC X(80)", "VARCHAR2(80)" },
            { "PIC X(100)", "VARCHAR2(100)" },
            { "PIC X(200)", "VARCHAR2(200)" },
            { "PIC X(500)", "VARCHAR2(500)" },
            { "PIC X(1000)", "VARCHAR2(1000)" },
            { "PIC X(2000)", "VARCHAR2(2000)" },
            { "PIC X(4000)", "VARCHAR2(4000)" },
            
            // Date types
            { "PIC 9(6)", "DATE" }, // YYMMDD
            { "PIC 9(8)", "DATE" }, // YYYYMMDD
            { "PIC 9(7)", "DATE" }, // YYYYDDD (Julian)
            
            // Special types
            { "PIC S9", "NUMBER" }, // Signed numeric
            { "PIC S9(5)V99", "NUMBER(7,2)" }, // Signed decimal
            { "COMP", "NUMBER" }, // Computational
            { "COMP-1", "BINARY_FLOAT" }, // Single precision
            { "COMP-2", "BINARY_DOUBLE" }, // Double precision
            { "COMP-3", "NUMBER" }, // Packed decimal
            { "COMP-4", "NUMBER" }, // Binary
            { "COMP-5", "NUMBER" }, // Native binary
        };

        /// <summary>
        /// Maps COBOL type to Oracle type
        /// </summary>
        public static string MapCobolToOracle(string cobolType)
        {
            if (string.IsNullOrWhiteSpace(cobolType))
                return "VARCHAR2(255)"; // Default fallback

            var normalizedType = cobolType.Trim().ToUpper();

            // Direct mapping
            if (_cobolToOracleMapping.TryGetValue(normalizedType, out var oracleType))
                return oracleType;

            // Pattern matching for variable lengths
            if (TryParseVariableLength(normalizedType, out var parsedType))
                return parsedType;

            // Fallback based on type patterns
            return InferOracleType(normalizedType);
        }

        /// <summary>
        /// Tries to parse variable length COBOL types
        /// </summary>
        private static bool TryParseVariableLength(string cobolType, out string oracleType)
        {
            oracleType = "VARCHAR2(255)"; // Default

            // PIC X(n) pattern
            var xMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+X\((\d+)\)");
            if (xMatch.Success && int.TryParse(xMatch.Groups[1].Value, out var xLength))
            {
                oracleType = xLength <= 10 ? $"CHAR({xLength})" : $"VARCHAR2({xLength})";
                return true;
            }

            // PIC 9(n) pattern
            var nMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+9\((\d+)\)");
            if (nMatch.Success && int.TryParse(nMatch.Groups[1].Value, out var nLength))
            {
                oracleType = $"NUMBER({nLength})";
                return true;
            }

            // PIC 9(n)V99 pattern
            var decimalMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+9\((\d+)\)V(\d+)");
            if (decimalMatch.Success && 
                int.TryParse(decimalMatch.Groups[1].Value, out var totalDigits) &&
                int.TryParse(decimalMatch.Groups[2].Value, out var decimalDigits))
            {
                oracleType = $"NUMBER({totalDigits},{decimalDigits})";
                return true;
            }

            return false;
        }

        /// <summary>
        /// Infers Oracle type based on COBOL type patterns
        /// </summary>
        private static string InferOracleType(string cobolType)
        {
            if (cobolType.Contains("9") && cobolType.Contains("V"))
                return "NUMBER(10,2)"; // Decimal
            else if (cobolType.Contains("9"))
                return "NUMBER(10)"; // Numeric
            else if (cobolType.Contains("X"))
                return "VARCHAR2(255)"; // Character
            else if (cobolType.Contains("DATE") || cobolType.Contains("TIME"))
                return "DATE"; // Date/Time
            else
                return "VARCHAR2(255)"; // Default
        }

        /// <summary>
        /// Validates field definition and returns validation messages
        /// </summary>
        public static List<string> ValidateFieldDefinition(FieldDefinition field, List<FieldDefinition> allFields)
        {
            var errors = new List<string>();

            // Check for missing field name
            if (string.IsNullOrWhiteSpace(field.FieldName))
                errors.Add("Field name is required");

            // Check for duplicate field names
            var duplicates = allFields.Where(f => f != field && 
                string.Equals(f.FieldName, field.FieldName, StringComparison.OrdinalIgnoreCase));
            if (duplicates.Any())
                errors.Add($"Duplicate field name: {field.FieldName}");

            // Check position conflicts
            if (field.StartPosition.HasValue && field.EndPosition.HasValue)
            {
                if (field.StartPosition > field.EndPosition)
                    errors.Add($"Start position ({field.StartPosition}) cannot be greater than end position ({field.EndPosition})");

                // Check for overlapping positions
                var overlaps = allFields.Where(f => f != field && 
                    f.StartPosition.HasValue && f.EndPosition.HasValue &&
                    !(field.EndPosition < f.StartPosition || field.StartPosition > f.EndPosition));
                if (overlaps.Any())
                    errors.Add($"Position overlaps with field(s): {string.Join(", ", overlaps.Select(f => f.FieldName))}");
            }

            // Check length validity
            if (field.Length.HasValue && field.Length <= 0)
                errors.Add("Length must be greater than 0");

            // Check data type validity
            if (!string.IsNullOrEmpty(field.SqlType))
            {
                var validTypes = new[] { "CHAR", "VARCHAR2", "NUMBER", "DATE", "TIMESTAMP", "CLOB", "BLOB" };
                if (!validTypes.Any(t => field.SqlType.StartsWith(t, StringComparison.OrdinalIgnoreCase)))
                    errors.Add($"Invalid SQL type: {field.SqlType}");
            }

            return errors;
        }

        /// <summary>
        /// Auto-calculates missing positions based on order and length
        /// </summary>
        public static void AutoCalculatePositions(List<FieldDefinition> fields)
        {
            var orderedFields = fields.Where(f => f.Order.HasValue).OrderBy(f => f.Order).ToList();
            int currentPosition = 1;

            foreach (var field in orderedFields)
            {
                if (!field.StartPosition.HasValue)
                {
                    field.StartPosition = currentPosition;
                }

                if (!field.EndPosition.HasValue && field.Length.HasValue)
                {
                    field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                }

                if (field.EndPosition.HasValue)
                {
                    currentPosition = field.EndPosition.Value + 1;
                }
                else if (field.Length.HasValue)
                {
                    currentPosition = field.StartPosition.Value + field.Length.Value;
                }
            }
        }

        /// <summary>
        /// Gets suggested data format for date fields
        /// </summary>
        public static string GetSuggestedDateFormat(string cobolType)
        {
            if (string.IsNullOrWhiteSpace(cobolType))
                return string.Empty;

            var normalizedType = cobolType.Trim().ToUpper();

            return normalizedType switch
            {
                "PIC 9(6)" => "YYMMDD",
                "PIC 9(8)" => "YYYYMMDD",
                "PIC 9(7)" => "YYYYDDD", // Julian date
                _ => string.Empty
            };
        }

        /// <summary>
        /// Gets suggested null value for COBOL types
        /// </summary>
        public static string GetSuggestedNullValue(string cobolType)
        {
            if (string.IsNullOrWhiteSpace(cobolType))
                return string.Empty;

            var normalizedType = cobolType.Trim().ToUpper();

            if (normalizedType.Contains("X"))
                return "BLANKS";
            else if (normalizedType.Contains("9"))
                return "99999";
            else
                return string.Empty;
        }
    }
} 