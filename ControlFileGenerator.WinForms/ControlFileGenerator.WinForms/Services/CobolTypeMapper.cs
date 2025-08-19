using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Services
{
    public static class CobolTypeMapper
    {
        /// <summary>
        /// COBOL to Oracle type mapping dictionary with comprehensive coverage
        /// </summary>
        private static readonly Dictionary<string, string> _cobolToOracleMapping = new(StringComparer.OrdinalIgnoreCase)
        {
            // Basic numeric types
            { "PIC 9", "NUMBER(1)" },
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
            { "PIC 9(21)", "NUMBER(21)" },
            { "PIC 9(22)", "NUMBER(22)" },
            { "PIC 9(23)", "NUMBER(23)" },
            { "PIC 9(24)", "NUMBER(24)" },
            { "PIC 9(25)", "NUMBER(25)" },
            { "PIC 9(26)", "NUMBER(26)" },
            { "PIC 9(27)", "NUMBER(27)" },
            { "PIC 9(28)", "NUMBER(28)" },
            { "PIC 9(29)", "NUMBER(29)" },
            { "PIC 9(30)", "NUMBER(30)" },
            { "PIC 9(31)", "NUMBER(31)" },
            { "PIC 9(32)", "NUMBER(32)" },
            { "PIC 9(33)", "NUMBER(33)" },
            { "PIC 9(34)", "NUMBER(34)" },
            { "PIC 9(35)", "NUMBER(35)" },
            { "PIC 9(36)", "NUMBER(36)" },
            { "PIC 9(37)", "NUMBER(37)" },
            { "PIC 9(38)", "NUMBER(38)" },
            
            // Signed numeric types
            { "PIC S9", "NUMBER(1)" },
            { "PIC S9(1)", "NUMBER(1)" },
            { "PIC S9(2)", "NUMBER(2)" },
            { "PIC S9(3)", "NUMBER(3)" },
            { "PIC S9(4)", "NUMBER(4)" },
            { "PIC S9(5)", "NUMBER(5)" },
            { "PIC S9(6)", "NUMBER(6)" },
            { "PIC S9(7)", "NUMBER(7)" },
            { "PIC S9(8)", "NUMBER(8)" },
            { "PIC S9(9)", "NUMBER(9)" },
            { "PIC S9(10)", "NUMBER(10)" },
            { "PIC S9(11)", "NUMBER(11)" },
            { "PIC S9(12)", "NUMBER(12)" },
            { "PIC S9(13)", "NUMBER(13)" },
            { "PIC S9(14)", "NUMBER(14)" },
            { "PIC S9(15)", "NUMBER(15)" },
            { "PIC S9(16)", "NUMBER(16)" },
            { "PIC S9(17)", "NUMBER(17)" },
            { "PIC S9(18)", "NUMBER(18)" },
            { "PIC S9(19)", "NUMBER(19)" },
            { "PIC S9(20)", "NUMBER(20)" },
            { "PIC S9(21)", "NUMBER(21)" },
            { "PIC S9(22)", "NUMBER(22)" },
            { "PIC S9(23)", "NUMBER(23)" },
            { "PIC S9(24)", "NUMBER(24)" },
            { "PIC S9(25)", "NUMBER(25)" },
            { "PIC S9(26)", "NUMBER(26)" },
            { "PIC S9(27)", "NUMBER(27)" },
            { "PIC S9(28)", "NUMBER(28)" },
            { "PIC S9(29)", "NUMBER(29)" },
            { "PIC S9(30)", "NUMBER(30)" },
            { "PIC S9(31)", "NUMBER(31)" },
            { "PIC S9(32)", "NUMBER(32)" },
            { "PIC S9(33)", "NUMBER(33)" },
            { "PIC S9(34)", "NUMBER(34)" },
            { "PIC S9(35)", "NUMBER(35)" },
            { "PIC S9(36)", "NUMBER(36)" },
            { "PIC S9(37)", "NUMBER(37)" },
            { "PIC S9(38)", "NUMBER(38)" },
            
            // Decimal types with various precision
            { "PIC 9(3)V99", "NUMBER(5,2)" },
            { "PIC 9(4)V99", "NUMBER(6,2)" },
            { "PIC 9(5)V99", "NUMBER(7,2)" },
            { "PIC 9(6)V99", "NUMBER(8,2)" },
            { "PIC 9(7)V99", "NUMBER(9,2)" },
            { "PIC 9(8)V99", "NUMBER(10,2)" },
            { "PIC 9(9)V99", "NUMBER(11,2)" },
            { "PIC 9(10)V99", "NUMBER(12,2)" },
            { "PIC 9(11)V99", "NUMBER(13,2)" },
            { "PIC 9(12)V99", "NUMBER(14,2)" },
            { "PIC 9(13)V99", "NUMBER(15,2)" },
            { "PIC 9(14)V99", "NUMBER(16,2)" },
            { "PIC 9(15)V99", "NUMBER(17,2)" },
            { "PIC 9(16)V99", "NUMBER(18,2)" },
            { "PIC 9(17)V99", "NUMBER(19,2)" },
            { "PIC 9(18)V99", "NUMBER(20,2)" },
            { "PIC 9(19)V99", "NUMBER(21,2)" },
            { "PIC 9(20)V99", "NUMBER(22,2)" },
            { "PIC 9(21)V99", "NUMBER(23,2)" },
            { "PIC 9(22)V99", "NUMBER(24,2)" },
            { "PIC 9(23)V99", "NUMBER(25,2)" },
            { "PIC 9(24)V99", "NUMBER(26,2)" },
            { "PIC 9(25)V99", "NUMBER(27,2)" },
            { "PIC 9(26)V99", "NUMBER(28,2)" },
            { "PIC 9(27)V99", "NUMBER(29,2)" },
            { "PIC 9(28)V99", "NUMBER(30,2)" },
            { "PIC 9(29)V99", "NUMBER(31,2)" },
            { "PIC 9(30)V99", "NUMBER(32,2)" },
            { "PIC 9(31)V99", "NUMBER(33,2)" },
            { "PIC 9(32)V99", "NUMBER(34,2)" },
            { "PIC 9(33)V99", "NUMBER(35,2)" },
            { "PIC 9(34)V99", "NUMBER(36,2)" },
            { "PIC 9(35)V99", "NUMBER(37,2)" },
            { "PIC 9(36)V99", "NUMBER(38,2)" },
            { "PIC 9(37)V99", "NUMBER(39,2)" },
            { "PIC 9(38)V99", "NUMBER(40,2)" },
            
            // Signed decimal types
            { "PIC S9(3)V99", "NUMBER(5,2)" },
            { "PIC S9(4)V99", "NUMBER(6,2)" },
            { "PIC S9(5)V99", "NUMBER(7,2)" },
            { "PIC S9(6)V99", "NUMBER(8,2)" },
            { "PIC S9(7)V99", "NUMBER(9,2)" },
            { "PIC S9(8)V99", "NUMBER(10,2)" },
            { "PIC S9(9)V99", "NUMBER(11,2)" },
            { "PIC S9(10)V99", "NUMBER(12,2)" },
            { "PIC S9(11)V99", "NUMBER(13,2)" },
            { "PIC S9(12)V99", "NUMBER(14,2)" },
            { "PIC S9(13)V99", "NUMBER(15,2)" },
            { "PIC S9(14)V99", "NUMBER(16,2)" },
            { "PIC S9(15)V99", "NUMBER(17,2)" },
            { "PIC S9(16)V99", "NUMBER(18,2)" },
            { "PIC S9(17)V99", "NUMBER(19,2)" },
            { "PIC S9(18)V99", "NUMBER(20,2)" },
            { "PIC S9(19)V99", "NUMBER(21,2)" },
            { "PIC S9(20)V99", "NUMBER(22,2)" },
            { "PIC S9(21)V99", "NUMBER(23,2)" },
            { "PIC S9(22)V99", "NUMBER(24,2)" },
            { "PIC S9(23)V99", "NUMBER(25,2)" },
            { "PIC S9(24)V99", "NUMBER(26,2)" },
            { "PIC S9(25)V99", "NUMBER(27,2)" },
            { "PIC S9(26)V99", "NUMBER(28,2)" },
            { "PIC S9(27)V99", "NUMBER(29,2)" },
            { "PIC S9(28)V99", "NUMBER(30,2)" },
            { "PIC S9(29)V99", "NUMBER(31,2)" },
            { "PIC S9(30)V99", "NUMBER(32,2)" },
            { "PIC S9(31)V99", "NUMBER(33,2)" },
            { "PIC S9(32)V99", "NUMBER(34,2)" },
            { "PIC S9(33)V99", "NUMBER(35,2)" },
            { "PIC S9(34)V99", "NUMBER(36,2)" },
            { "PIC S9(35)V99", "NUMBER(37,2)" },
            { "PIC S9(36)V99", "NUMBER(38,2)" },
            { "PIC S9(37)V99", "NUMBER(39,2)" },
            { "PIC S9(38)V99", "NUMBER(40,2)" },
            
            // Three decimal places
            { "PIC 9(5)V999", "NUMBER(8,3)" },
            { "PIC 9(6)V999", "NUMBER(9,3)" },
            { "PIC 9(7)V999", "NUMBER(10,3)" },
            { "PIC 9(8)V999", "NUMBER(11,3)" },
            { "PIC 9(9)V999", "NUMBER(12,3)" },
            { "PIC 9(10)V999", "NUMBER(13,3)" },
            { "PIC 9(11)V999", "NUMBER(14,3)" },
            { "PIC 9(12)V999", "NUMBER(15,3)" },
            { "PIC 9(13)V999", "NUMBER(16,3)" },
            { "PIC 9(14)V999", "NUMBER(17,3)" },
            { "PIC 9(15)V999", "NUMBER(18,3)" },
            { "PIC 9(16)V999", "NUMBER(19,3)" },
            { "PIC 9(17)V999", "NUMBER(20,3)" },
            { "PIC 9(18)V999", "NUMBER(21,3)" },
            { "PIC 9(19)V999", "NUMBER(22,3)" },
            { "PIC 9(20)V999", "NUMBER(23,3)" },
            { "PIC 9(21)V999", "NUMBER(24,3)" },
            { "PIC 9(22)V999", "NUMBER(25,3)" },
            { "PIC 9(23)V999", "NUMBER(26,3)" },
            { "PIC 9(24)V999", "NUMBER(27,3)" },
            { "PIC 9(25)V999", "NUMBER(28,3)" },
            { "PIC 9(26)V999", "NUMBER(29,3)" },
            { "PIC 9(27)V999", "NUMBER(30,3)" },
            { "PIC 9(28)V999", "NUMBER(31,3)" },
            { "PIC 9(29)V999", "NUMBER(32,3)" },
            { "PIC 9(30)V999", "NUMBER(33,3)" },
            { "PIC 9(31)V999", "NUMBER(34,3)" },
            { "PIC 9(32)V999", "NUMBER(35,3)" },
            { "PIC 9(33)V999", "NUMBER(36,3)" },
            { "PIC 9(34)V999", "NUMBER(37,3)" },
            { "PIC 9(35)V999", "NUMBER(38,3)" },
            { "PIC 9(36)V999", "NUMBER(39,3)" },
            { "PIC 9(37)V999", "NUMBER(40,3)" },
            { "PIC 9(38)V999", "NUMBER(41,3)" },
            
            // Signed three decimal places
            { "PIC S9(5)V999", "NUMBER(8,3)" },
            { "PIC S9(6)V999", "NUMBER(9,3)" },
            { "PIC S9(7)V999", "NUMBER(10,3)" },
            { "PIC S9(8)V999", "NUMBER(11,3)" },
            { "PIC S9(9)V999", "NUMBER(12,3)" },
            { "PIC S9(10)V999", "NUMBER(13,3)" },
            { "PIC S9(11)V999", "NUMBER(14,3)" },
            { "PIC S9(12)V999", "NUMBER(15,3)" },
            { "PIC S9(13)V999", "NUMBER(16,3)" },
            { "PIC S9(14)V999", "NUMBER(17,3)" },
            { "PIC S9(15)V999", "NUMBER(18,3)" },
            { "PIC S9(16)V999", "NUMBER(19,3)" },
            { "PIC S9(17)V999", "NUMBER(20,3)" },
            { "PIC S9(18)V999", "NUMBER(21,3)" },
            { "PIC S9(19)V999", "NUMBER(22,3)" },
            { "PIC S9(20)V999", "NUMBER(23,3)" },
            { "PIC S9(21)V999", "NUMBER(24,3)" },
            { "PIC S9(22)V999", "NUMBER(25,3)" },
            { "PIC S9(23)V999", "NUMBER(26,3)" },
            { "PIC S9(24)V999", "NUMBER(27,3)" },
            { "PIC S9(25)V999", "NUMBER(28,3)" },
            { "PIC S9(26)V999", "NUMBER(29,3)" },
            { "PIC S9(27)V999", "NUMBER(30,3)" },
            { "PIC S9(28)V999", "NUMBER(31,3)" },
            { "PIC S9(29)V999", "NUMBER(32,3)" },
            { "PIC S9(30)V999", "NUMBER(33,3)" },
            { "PIC S9(31)V999", "NUMBER(34,3)" },
            { "PIC S9(32)V999", "NUMBER(35,3)" },
            { "PIC S9(33)V999", "NUMBER(36,3)" },
            { "PIC S9(34)V999", "NUMBER(37,3)" },
            { "PIC S9(35)V999", "NUMBER(38,3)" },
            { "PIC S9(36)V999", "NUMBER(39,3)" },
            { "PIC S9(37)V999", "NUMBER(40,3)" },
            { "PIC S9(38)V999", "NUMBER(41,3)" },
            
            // Character types - optimized for length
            { "PIC X", "CHAR(1)" },
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
            { "COMP", "NUMBER" }, // Computational
            { "COMP-1", "BINARY_FLOAT" }, // Single precision
            { "COMP-2", "BINARY_DOUBLE" }, // Double precision
            { "COMP-3", "NUMBER" }, // Packed decimal
            { "COMP-4", "NUMBER" }, // Binary
            { "COMP-5", "NUMBER" }, // Native binary
        };

        /// <summary>
        /// Maps COBOL type to Oracle type with comprehensive edge case handling
        /// </summary>
        public static string MapCobolToOracle(string cobolType)
        {
            if (string.IsNullOrWhiteSpace(cobolType))
                return "VARCHAR2(255)"; // Default fallback for missing SQL type

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
        /// Tries to parse variable length COBOL types with enhanced pattern matching
        /// </summary>
        private static bool TryParseVariableLength(string cobolType, out string oracleType)
        {
            oracleType = "VARCHAR2(255)"; // Default

            // PIC X(n) pattern - character fields
            var xMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+X\((\d+)\)");
            if (xMatch.Success && int.TryParse(xMatch.Groups[1].Value, out var xLength))
            {
                oracleType = xLength <= 10 ? $"CHAR({xLength})" : $"VARCHAR2({xLength})";
                return true;
            }

            // PIC 9(n) pattern - numeric fields
            var nMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+9\((\d+)\)");
            if (nMatch.Success && int.TryParse(nMatch.Groups[1].Value, out var nLength))
            {
                oracleType = $"NUMBER({nLength})";
                return true;
            }

            // PIC S9(n) pattern - signed numeric fields
            var snMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+S9\((\d+)\)");
            if (snMatch.Success && int.TryParse(snMatch.Groups[1].Value, out var snLength))
            {
                oracleType = $"NUMBER({snLength})";
                return true;
            }

            // PIC 9(n)V(m) pattern - decimal fields (fraction without parentheses, e.g. V99)
            var decimalMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+9\((\d+)\)V(\d+)");
            if (decimalMatch.Success && 
                int.TryParse(decimalMatch.Groups[1].Value, out var totalDigits) &&
                int.TryParse(decimalMatch.Groups[2].Value, out var decimalDigits))
            {
                oracleType = $"NUMBER({totalDigits},{decimalDigits})";
                return true;
            }

            // PIC 9(n)V9(m) pattern - decimal fields (fraction with parentheses, e.g. V9(2))
            var decimalMatchParen = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+9\((\d+)\)\s*V\s*9\((\d+)\)");
            if (decimalMatchParen.Success &&
                int.TryParse(decimalMatchParen.Groups[1].Value, out var wholeDigitsParen) &&
                int.TryParse(decimalMatchParen.Groups[2].Value, out var fracDigitsParen))
            {
                oracleType = $"NUMBER({wholeDigitsParen + fracDigitsParen},{fracDigitsParen})";
                return true;
            }

            // PIC S9(n)V(m) pattern - signed decimal fields (fraction without parentheses)
            var sDecimalMatch = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+S9\((\d+)\)V(\d+)");
            if (sDecimalMatch.Success && 
                int.TryParse(sDecimalMatch.Groups[1].Value, out var sTotalDigits) &&
                int.TryParse(sDecimalMatch.Groups[2].Value, out var sDecimalDigits))
            {
                oracleType = $"NUMBER({sTotalDigits},{sDecimalDigits})";
                return true;
            }

            // PIC S9(n)V9(m) pattern - signed decimal fields (fraction with parentheses)
            var sDecimalMatchParen = System.Text.RegularExpressions.Regex.Match(cobolType, @"PIC\s+S9\((\d+)\)\s*V\s*9\((\d+)\)");
            if (sDecimalMatchParen.Success &&
                int.TryParse(sDecimalMatchParen.Groups[1].Value, out var sWholeDigitsParen) &&
                int.TryParse(sDecimalMatchParen.Groups[2].Value, out var sFracDigitsParen))
            {
                oracleType = $"NUMBER({sWholeDigitsParen + sFracDigitsParen},{sFracDigitsParen})";
                return true;
            }

            return false;
        }

        /// <summary>
        /// Infers Oracle type based on COBOL type patterns with enhanced logic
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
            else if (cobolType.Contains("COMP"))
                return "NUMBER"; // Computational
            else
                return "VARCHAR2(255)"; // Default fallback
        }

        /// <summary>
        /// Validates field definition and returns comprehensive validation messages
        /// </summary>
        public static List<string> ValidateFieldDefinition(FieldDefinition field, List<FieldDefinition> allFields)
        {
            var errors = new List<string>();

            // Check for missing field name
            if (string.IsNullOrWhiteSpace(field.FieldName))
                errors.Add("Field name is required");

            // Check for duplicate field names (case-insensitive)
            var duplicates = allFields.Where(f => f != field && 
                string.Equals(f.FieldName, field.FieldName, StringComparison.OrdinalIgnoreCase));
            if (duplicates.Any())
                errors.Add($"Duplicate field name: {field.FieldName}");

            // Handle virtual fields differently
            if (field.IsVirtual)
            {
                // Virtual fields don't need position validation
                // But they need either Default Value or Transform
                if (string.IsNullOrEmpty(field.DefaultValue) && string.IsNullOrEmpty(field.Transform))
                {
                    errors.Add($"Virtual field '{field.FieldName}' must have either a Default Value or Transform expression");
                }
            }
            else
            {
                // Non-virtual fields need position validation
                // Check position conflicts
                if (field.StartPosition.HasValue && field.EndPosition.HasValue)
                {
                    if (field.StartPosition > field.EndPosition)
                        errors.Add($"Start position ({field.StartPosition}) cannot be greater than end position ({field.EndPosition})");

                    // Check for overlapping positions (only with non-virtual fields)
                    var overlaps = allFields.Where(f => f != field && !f.IsVirtual &&
                        f.StartPosition.HasValue && f.EndPosition.HasValue &&
                        !(field.EndPosition < f.StartPosition || field.StartPosition > f.EndPosition));
                    if (overlaps.Any())
                        errors.Add($"Position overlaps with field(s): {string.Join(", ", overlaps.Select(f => f.FieldName))}");
                }

                // Check length validity
                if (field.Length.HasValue && field.Length <= 0)
                    errors.Add("Length must be greater than 0");
            }

            // Check data type validity
            if (!string.IsNullOrEmpty(field.SqlType))
            {
                var validTypes = new[] { "CHAR", "VARCHAR2", "NUMBER", "DATE", "TIMESTAMP", "CLOB", "BLOB", "BINARY_FLOAT", "BINARY_DOUBLE" };
                if (!validTypes.Any(t => field.SqlType.StartsWith(t, StringComparison.OrdinalIgnoreCase)))
                    errors.Add($"Invalid SQL type: {field.SqlType}");
            }

            return errors;
        }

        /// <summary>
        /// Auto-calculates missing positions based on order and length with enhanced logic
        /// </summary>
        public static void AutoCalculatePositions(List<FieldDefinition> fields)
        {
            var orderedFields = fields.Where(f => f.Order.HasValue).OrderBy(f => f.Order).ToList();
            int currentPosition = 1;

            foreach (var field in orderedFields)
            {
                // Calculate start position if missing
                if (!field.StartPosition.HasValue)
                {
                    field.StartPosition = currentPosition;
                }

                // Calculate end position if missing but length is available
                if (!field.EndPosition.HasValue && field.Length.HasValue)
                {
                    field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                }

                // Calculate length if missing but positions are available
                if (!field.Length.HasValue && field.StartPosition.HasValue && field.EndPosition.HasValue)
                {
                    field.Length = field.EndPosition.Value - field.StartPosition.Value + 1;
                }

                // Update current position for next field
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

        /// <summary>
        /// Handles edge cases for missing SQL types by inferring from COBOL
        /// </summary>
        public static string HandleMissingSqlType(string cobolType)
        {
            if (string.IsNullOrWhiteSpace(cobolType))
                return "VARCHAR2(255)"; // Default fallback

            return MapCobolToOracle(cobolType);
        }

        /// <summary>
        /// Handles missing start/end positions by calculating from order and length
        /// </summary>
        public static void HandleMissingPositions(FieldDefinition field, List<FieldDefinition> allFields)
        {
            if (!field.StartPosition.HasValue && field.Order.HasValue)
            {
                var previousFields = allFields.Where(f => f.Order.HasValue && f.Order < field.Order)
                    .OrderBy(f => f.Order).ToList();
                
                int startPos = 1;
                foreach (var prevField in previousFields)
                {
                    if (prevField.EndPosition.HasValue)
                        startPos = prevField.EndPosition.Value + 1;
                    else if (prevField.Length.HasValue && prevField.StartPosition.HasValue)
                        startPos = prevField.StartPosition.Value + prevField.Length.Value;
                }
                
                field.StartPosition = startPos;
            }

            if (!field.EndPosition.HasValue && field.StartPosition.HasValue && field.Length.HasValue)
            {
                field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
            }
        }

        /// <summary>
        /// Detects overlapping positions and returns warning messages
        /// </summary>
        public static List<string> DetectOverlappingPositions(List<FieldDefinition> fields)
        {
            var warnings = new List<string>();
            var sortedFields = fields.Where(f => f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition).ToList();

            for (int i = 0; i < sortedFields.Count - 1; i++)
            {
                var current = sortedFields[i];
                var next = sortedFields[i + 1];

                if (current.EndPosition >= next.StartPosition)
                {
                    warnings.Add($"Position overlap detected between '{current.FieldName}' and '{next.FieldName}'");
                }
            }

            return warnings;
        }

        /// <summary>
        /// Detects duplicate field names and returns warning messages
        /// </summary>
        public static List<string> DetectDuplicateFieldNames(List<FieldDefinition> fields)
        {
            var warnings = new List<string>();
            var fieldNames = fields.Select(f => f.FieldName.ToLower()).ToList();
            var duplicates = fieldNames.GroupBy(x => x).Where(g => g.Count() > 1).Select(g => g.Key);

            foreach (var duplicate in duplicates)
            {
                warnings.Add($"Duplicate field name detected: {duplicate}");
            }

            return warnings;
        }
    }
} 