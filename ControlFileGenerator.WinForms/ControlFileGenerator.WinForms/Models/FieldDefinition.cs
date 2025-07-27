using System.ComponentModel;
using ControlFileGenerator.WinForms.Services;

namespace ControlFileGenerator.WinForms.Models
{
    public class FieldDefinition
    {
        [DisplayName("Field Name")]
        public string FieldName { get; set; } = string.Empty;

        [DisplayName("Virtual Field")]
        public bool IsVirtual { get; set; } = false;

        [DisplayName("Order")]
        public int? Order { get; set; }

        [DisplayName("Start Position")]
        public int? StartPosition { get; set; }

        [DisplayName("End Position")]
        public int? EndPosition { get; set; }

        [DisplayName("Length")]
        public int? Length { get; set; }

        [DisplayName("COBOL Type")]
        public string CobolType { get; set; } = string.Empty;

        [DisplayName("SQL Type")]
        public string SqlType { get; set; } = string.Empty;

        [DisplayName("Nullable")]
        public string Nullable { get; set; } = "YES";

        [DisplayName("Transform")]
        public string Transform { get; set; } = string.Empty;

        [DisplayName("Default Value")]
        public string DefaultValue { get; set; } = string.Empty;

        [DisplayName("Null If Value")]
        public string NullIfValue { get; set; } = string.Empty;

        [DisplayName("Enclosed By")]
        public string EnclosedBy { get; set; } = string.Empty;

        [DisplayName("Delimiter")]
        public string Delimiter { get; set; } = string.Empty;

        [DisplayName("Data Format")]
        public string DataFormat { get; set; } = string.Empty;

        [DisplayName("Description")]
        public string Description { get; set; } = string.Empty;

        /// <summary>
        /// Calculates the position string for SQL*Loader control file
        /// </summary>
        public string GetPositionString()
        {
            // Virtual fields don't need position information
            if (IsVirtual)
            {
                return string.Empty;
            }

            // Case 1: Both start and end positions are specified
            if (StartPosition.HasValue && EndPosition.HasValue)
            {
                return $"POSITION({StartPosition}:{EndPosition})";
            }
            
            // Case 2: Start position and length are specified
            else if (StartPosition.HasValue && Length.HasValue)
            {
                var calculatedEndPosition = StartPosition.Value + Length.Value - 1;
                return $"POSITION({StartPosition}:{calculatedEndPosition})";
            }
            
            // Case 3: Only length is specified (for delimited files)
            else if (Length.HasValue && !StartPosition.HasValue && !EndPosition.HasValue)
            {
                return string.Empty; // Delimited files don't use POSITION
            }
            
            // Case 4: Order and length are specified (for auto-calculation)
            else if (Order.HasValue && Length.HasValue)
            {
                var calculatedStartPosition = GetCalculatedStartPosition(); // Note: This will use fallback logic
                var calculatedEndPosition = calculatedStartPosition + Length.Value - 1;
                return $"POSITION({calculatedStartPosition}:{calculatedEndPosition})";
            }
            
            // Case 5: No position information available
            return string.Empty;
        }

        /// <summary>
        /// Gets the calculated start position based on order and previous fields
        /// This method requires the complete list of fields to calculate properly
        /// </summary>
        public int GetCalculatedStartPosition(List<FieldDefinition>? allFields = null)
        {
            // If we have explicit start position, use it
            if (StartPosition.HasValue)
            {
                return StartPosition.Value;
            }

            // If no order specified, default to position 1
            if (!Order.HasValue)
            {
                return 1;
            }

            // If no field list provided, we can't calculate based on previous fields
            if (allFields == null || allFields.Count == 0)
            {
                return Order.Value;
            }

            // Calculate position based on previous fields
            var previousFields = allFields
                .Where(f => !f.IsVirtual && f.Order.HasValue && f.Order < Order.Value)
                .OrderBy(f => f.Order)
                .ToList();

            if (previousFields.Count == 0)
            {
                return 1; // First field starts at position 1
            }

            // Find the end position of the last previous field
            var lastPreviousField = previousFields.Last();
            var lastEndPosition = GetFieldEndPosition(lastPreviousField);
            
            return lastEndPosition + 1;
        }

        /// <summary>
        /// Gets the end position of a field, calculating it if necessary
        /// </summary>
        private int GetFieldEndPosition(FieldDefinition field)
        {
            // If end position is explicitly set, use it
            if (field.EndPosition.HasValue)
            {
                return field.EndPosition.Value;
            }

            // If start position and length are set, calculate end position
            if (field.StartPosition.HasValue && field.Length.HasValue)
            {
                return field.StartPosition.Value + field.Length.Value - 1;
            }

            // If only length is set, try to infer from COBOL type
            if (field.Length.HasValue)
            {
                return field.Length.Value; // Assume it starts at position 1 if no start position
            }

            // Try to calculate length from COBOL type
            var inferredLength = CalculateLengthFromCobolType(field.CobolType);
            if (inferredLength > 0)
            {
                return inferredLength; // Assume it starts at position 1
            }

            // Default fallback
            return 1;
        }

        /// <summary>
        /// Calculates field length from COBOL type definition
        /// </summary>
        public int CalculateLengthFromCobolType(string cobolType)
        {
            if (string.IsNullOrWhiteSpace(cobolType))
                return 0;

            var normalizedType = cobolType.Trim().ToUpper();
            
            // Handle PIC X(n) - Character fields
            var xMatch = System.Text.RegularExpressions.Regex.Match(normalizedType, @"PIC\s+X\((\d+)\)");
            if (xMatch.Success && int.TryParse(xMatch.Groups[1].Value, out int xLength))
            {
                return xLength;
            }

            // Handle PIC X - Single character
            if (normalizedType == "PIC X")
            {
                return 1;
            }

            // Handle PIC 9(n) - Numeric fields
            var nineMatch = System.Text.RegularExpressions.Regex.Match(normalizedType, @"PIC\s+9\((\d+)\)");
            if (nineMatch.Success && int.TryParse(nineMatch.Groups[1].Value, out int nineLength))
            {
                return nineLength;
            }

            // Handle PIC 9 - Single digit
            if (normalizedType == "PIC 9")
            {
                return 1;
            }

            // Handle PIC S9(n) - Signed numeric fields
            var sNineMatch = System.Text.RegularExpressions.Regex.Match(normalizedType, @"PIC\s+S9\((\d+)\)");
            if (sNineMatch.Success && int.TryParse(sNineMatch.Groups[1].Value, out int sNineLength))
            {
                return sNineLength;
            }

            // Handle PIC S9 - Signed single digit
            if (normalizedType == "PIC S9")
            {
                return 1;
            }

            // Handle decimal types like PIC 9(n)V99
            var decimalMatch = System.Text.RegularExpressions.Regex.Match(normalizedType, @"PIC\s+[S]?9\((\d+)\)V(\d+)");
            if (decimalMatch.Success && int.TryParse(decimalMatch.Groups[1].Value, out int wholeDigits))
            {
                return wholeDigits; // Return whole digits part
            }

            // Handle simple decimal types like PIC 9V99
            var simpleDecimalMatch = System.Text.RegularExpressions.Regex.Match(normalizedType, @"PIC\s+[S]?9V(\d+)");
            if (simpleDecimalMatch.Success)
            {
                return 1; // Single digit before decimal
            }

            // Handle date formats
            if (normalizedType.Contains("PIC 9(8)"))
            {
                return 8; // YYYYMMDD format
            }
            if (normalizedType.Contains("PIC 9(6)"))
            {
                return 6; // YYMMDD format
            }
            if (normalizedType.Contains("PIC 9(7)"))
            {
                return 7; // YYYYDDD (Julian) format
            }

            return 0; // Unknown type
        }

        /// <summary>
        /// Gets the Oracle data type for the control file
        /// </summary>
        public string GetOracleDataType()
        {
            // Use explicit SQL type if provided
            if (!string.IsNullOrEmpty(SqlType))
            {
                return SqlType.ToUpper();
            }

            // Infer from COBOL type using the centralized mapper
            if (!string.IsNullOrEmpty(CobolType))
            {
                return CobolTypeMapper.MapCobolToOracle(CobolType);
            }

            // Default to CHAR if no type information
            return "CHAR";
        }

        /// <summary>
        /// Gets the format string for the control file
        /// </summary>
        public string GetFormatString()
        {
            if (!string.IsNullOrEmpty(DataFormat))
            {
                return $"\"{DataFormat}\"";
            }

            // Infer format from data type
            var oracleType = GetOracleDataType();
            return oracleType switch
            {
                "DATE" => "\"YYYY-MM-DD\"",
                "TIMESTAMP" => "\"YYYY-MM-DD HH24:MI:SS\"",
                _ => string.Empty
            };
        }

        /// <summary>
        /// Gets the NULLIF clause if applicable
        /// </summary>
        public string GetNullIfClause()
        {
            if (!string.IsNullOrEmpty(NullIfValue))
            {
                return IntelligentNullValueProcessor.ProcessNullValuePattern(NullIfValue, FieldName);
            }
            return string.Empty;
        }

        /// <summary>
        /// Gets the transform expression if applicable
        /// </summary>
        public string GetTransformExpression()
        {
            if (!string.IsNullOrEmpty(Transform))
            {
                return $"\"{Transform}\"";
            }
            return string.Empty;
        }

        /// <summary>
        /// Gets the default value expression if applicable
        /// </summary>
        public string GetDefaultValueExpression()
        {
            if (!string.IsNullOrEmpty(DefaultValue))
            {
                return $"DEFAULTIF {FieldName}=BLANKS \"{DefaultValue}\"";
            }
            return string.Empty;
        }

        /// <summary>
        /// Gets the virtual field expression (CONSTANT or SQL expression)
        /// </summary>
        public string GetVirtualFieldExpression()
        {
            if (!IsVirtual)
            {
                return string.Empty;
            }

            // If Default Value is provided, use CONSTANT
            if (!string.IsNullOrEmpty(DefaultValue))
            {
                return $"CONSTANT '{DefaultValue}'";
            }

            // If Transform is provided, use SQL expression
            if (!string.IsNullOrEmpty(Transform))
            {
                return $"\"{Transform}\"";
            }

            return string.Empty;
        }

        /// <summary>
        /// Validates the field definition and returns validation errors
        /// </summary>
        public List<string> Validate(List<FieldDefinition>? allFields = null)
        {
            var errors = new List<string>();

            // Validate field name
            if (string.IsNullOrWhiteSpace(FieldName))
            {
                errors.Add("Field name is required");
            }
            else if (FieldName.Length > 30)
            {
                errors.Add("Field name cannot exceed 30 characters");
            }
            else if (!System.Text.RegularExpressions.Regex.IsMatch(FieldName, @"^[A-Za-z_][A-Za-z0-9_]*$"))
            {
                errors.Add("Field name must start with a letter or underscore and contain only letters, numbers, and underscores");
            }

            // Validate virtual field logic
            if (IsVirtual)
            {
                // Virtual fields don't need positions
                if (StartPosition.HasValue || EndPosition.HasValue)
                {
                    errors.Add("Virtual fields should not have explicit positions");
                }
            }
            else
            {
                // Non-virtual fields need some form of positioning
                if (!StartPosition.HasValue && !EndPosition.HasValue && !Length.HasValue && !Order.HasValue)
                {
                    errors.Add("Non-virtual fields must have either positions, length, or order specified");
                }
            }

            // Validate position consistency
            if (StartPosition.HasValue && EndPosition.HasValue)
            {
                if (StartPosition.Value > EndPosition.Value)
                {
                    errors.Add("Start position cannot be greater than end position");
                }
            }

            // Validate length consistency
            if (StartPosition.HasValue && EndPosition.HasValue && Length.HasValue)
            {
                var calculatedLength = EndPosition.Value - StartPosition.Value + 1;
                if (calculatedLength != Length.Value)
                {
                    errors.Add($"Length ({Length.Value}) does not match position range ({StartPosition.Value}-{EndPosition.Value})");
                }
            }

            // Validate order consistency
            if (Order.HasValue && Order.Value <= 0)
            {
                errors.Add("Order must be a positive integer");
            }

            // Validate SQL type
            if (!string.IsNullOrEmpty(SqlType))
            {
                var validTypes = new[] { "CHAR", "VARCHAR2", "NUMBER", "DATE", "TIMESTAMP", "CLOB", "BLOB", "RAW", "LONG", "LONG RAW" };
                var isValidType = validTypes.Any(t => SqlType.ToUpper().StartsWith(t));
                if (!isValidType)
                {
                    errors.Add($"Invalid SQL type: {SqlType}");
                }
            }

            // Validate nullable value
            if (!string.IsNullOrEmpty(Nullable))
            {
                var normalizedNullable = Nullable.ToUpper();
                if (normalizedNullable != "YES" && normalizedNullable != "NO")
                {
                    errors.Add("Nullable must be 'YES' or 'NO'");
                }
            }

            return errors;
        }

        /// <summary>
        /// Auto-calculates missing properties based on available information
        /// </summary>
        public void AutoCalculateMissingProperties(List<FieldDefinition>? allFields = null)
        {
            // Skip virtual fields
            if (IsVirtual)
                return;

            // Calculate length from COBOL type if not specified
            if (!Length.HasValue && !string.IsNullOrEmpty(CobolType))
            {
                Length = CalculateLengthFromCobolType(CobolType);
            }

            // Calculate end position from start position and length
            if (StartPosition.HasValue && Length.HasValue && !EndPosition.HasValue)
            {
                EndPosition = StartPosition.Value + Length.Value - 1;
            }

            // Calculate start position from end position and length
            if (EndPosition.HasValue && Length.HasValue && !StartPosition.HasValue)
            {
                StartPosition = EndPosition.Value - Length.Value + 1;
            }

            // Calculate length from start and end positions
            if (StartPosition.HasValue && EndPosition.HasValue && !Length.HasValue)
            {
                Length = EndPosition.Value - StartPosition.Value + 1;
            }

            // Infer SQL type from COBOL type if not specified
            if (string.IsNullOrEmpty(SqlType) && !string.IsNullOrEmpty(CobolType))
            {
                SqlType = CobolTypeMapper.MapCobolToOracle(CobolType);
            }
        }

        /// <summary>
        /// Creates a deep copy of the FieldDefinition
        /// </summary>
        public FieldDefinition Clone()
        {
            return new FieldDefinition
            {
                FieldName = this.FieldName,
                IsVirtual = this.IsVirtual,
                Order = this.Order,
                StartPosition = this.StartPosition,
                EndPosition = this.EndPosition,
                Length = this.Length,
                CobolType = this.CobolType,
                SqlType = this.SqlType,
                Nullable = this.Nullable,
                Transform = this.Transform,
                DefaultValue = this.DefaultValue,
                NullIfValue = this.NullIfValue,
                EnclosedBy = this.EnclosedBy,
                Delimiter = this.Delimiter,
                DataFormat = this.DataFormat,
                Description = this.Description
            };
        }

        /// <summary>
        /// Returns a string representation of the field definition
        /// </summary>
        public override string ToString()
        {
            var parts = new List<string>();
            
            if (!string.IsNullOrEmpty(FieldName))
                parts.Add($"Name: {FieldName}");
            
            if (IsVirtual)
                parts.Add("Virtual");
            
            if (Order.HasValue)
                parts.Add($"Order: {Order}");
            
            if (StartPosition.HasValue && EndPosition.HasValue)
                parts.Add($"Position: {StartPosition}-{EndPosition}");
            else if (StartPosition.HasValue)
                parts.Add($"Start: {StartPosition}");
            
            if (Length.HasValue)
                parts.Add($"Length: {Length}");
            
            if (!string.IsNullOrEmpty(SqlType))
                parts.Add($"Type: {SqlType}");
            
            return string.Join(", ", parts);
        }
    }
} 