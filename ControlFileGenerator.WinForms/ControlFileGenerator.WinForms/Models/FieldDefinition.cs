using System.ComponentModel;
using ControlFileGenerator.WinForms.Services;

namespace ControlFileGenerator.WinForms.Models
{
    public class FieldDefinition
    {
        [DisplayName("Field Name")]
        public string FieldName { get; set; } = string.Empty;

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
            if (StartPosition.HasValue && EndPosition.HasValue)
            {
                return $"POSITION({StartPosition}:{EndPosition})";
            }
            else if (StartPosition.HasValue && Length.HasValue)
            {
                return $"POSITION({StartPosition}:{StartPosition + Length - 1})";
            }
            else if (Order.HasValue && Length.HasValue)
            {
                // Calculate position based on order and previous fields
                return $"POSITION({GetCalculatedStartPosition()}:{GetCalculatedStartPosition() + Length - 1})";
            }
            
            return string.Empty;
        }

        /// <summary>
        /// Gets the calculated start position based on order
        /// </summary>
        private int GetCalculatedStartPosition()
        {
            // This would need to be calculated based on previous fields
            // For now, return a placeholder
            return Order ?? 1;
        }

        /// <summary>
        /// Gets the Oracle data type for the control file
        /// </summary>
        public string GetOracleDataType()
        {
            if (!string.IsNullOrEmpty(SqlType))
            {
                return SqlType.ToUpper();
            }

            // Infer from COBOL type if available
            if (!string.IsNullOrEmpty(CobolType))
            {
                return InferOracleTypeFromCobol(CobolType);
            }

            // Default to CHAR if no type information
            return "CHAR";
        }

        /// <summary>
        /// Infers Oracle data type from COBOL type
        /// </summary>
        private string InferOracleTypeFromCobol(string cobolType)
        {
            return cobolType.ToUpper() switch
            {
                "PIC X" or "PIC A" => "CHAR",
                "PIC 9" => "NUMBER",
                "PIC S9" => "NUMBER",
                "PIC 9V99" => "NUMBER",
                "PIC S9V99" => "NUMBER",
                "PIC 9(5)" => "NUMBER",
                "PIC S9(5)" => "NUMBER",
                "PIC 9(10)" => "NUMBER",
                "PIC S9(10)" => "NUMBER",
                "PIC 9(15)" => "NUMBER",
                "PIC S9(15)" => "NUMBER",
                "PIC 9(3)V99" => "NUMBER",
                "PIC S9(3)V99" => "NUMBER",
                "PIC 9(5)V99" => "NUMBER",
                "PIC S9(5)V99" => "NUMBER",
                "PIC 9(7)V99" => "NUMBER",
                "PIC S9(7)V99" => "NUMBER",
                "PIC 9(9)V99" => "NUMBER",
                "PIC S9(9)V99" => "NUMBER",
                "PIC 9(11)V99" => "NUMBER",
                "PIC S9(11)V99" => "NUMBER",
                "PIC 9(13)V99" => "NUMBER",
                "PIC S9(13)V99" => "NUMBER",
                "PIC 9(15)V99" => "NUMBER",
                "PIC S9(15)V99" => "NUMBER",
                "PIC 9(17)V99" => "NUMBER",
                "PIC S9(17)V99" => "NUMBER",
                "PIC 9(19)V99" => "NUMBER",
                "PIC S9(19)V99" => "NUMBER",
                "PIC 9(21)V99" => "NUMBER",
                "PIC S9(21)V99" => "NUMBER",
                "PIC 9(23)V99" => "NUMBER",
                "PIC S9(23)V99" => "NUMBER",
                "PIC 9(25)V99" => "NUMBER",
                "PIC S9(25)V99" => "NUMBER",
                "PIC 9(27)V99" => "NUMBER",
                "PIC S9(27)V99" => "NUMBER",
                "PIC 9(29)V99" => "NUMBER",
                "PIC S9(29)V99" => "NUMBER",
                "PIC 9(31)V99" => "NUMBER",
                "PIC S9(31)V99" => "NUMBER",
                "PIC 9(33)V99" => "NUMBER",
                "PIC S9(33)V99" => "NUMBER",
                "PIC 9(35)V99" => "NUMBER",
                "PIC S9(35)V99" => "NUMBER",
                "PIC 9(37)V99" => "NUMBER",
                "PIC S9(37)V99" => "NUMBER",
                "PIC 9(39)V99" => "NUMBER",
                "PIC S9(39)V99" => "NUMBER",
                "PIC 9(41)V99" => "NUMBER",
                "PIC S9(41)V99" => "NUMBER",
                "PIC 9(43)V99" => "NUMBER",
                "PIC S9(43)V99" => "NUMBER",
                "PIC 9(45)V99" => "NUMBER",
                "PIC S9(45)V99" => "NUMBER",
                "PIC 9(47)V99" => "NUMBER",
                "PIC S9(47)V99" => "NUMBER",
                "PIC 9(49)V99" => "NUMBER",
                "PIC S9(49)V99" => "NUMBER",
                "PIC 9(51)V99" => "NUMBER",
                "PIC S9(51)V99" => "NUMBER",
                "PIC 9(53)V99" => "NUMBER",
                "PIC S9(53)V99" => "NUMBER",
                "PIC 9(55)V99" => "NUMBER",
                "PIC S9(55)V99" => "NUMBER",
                "PIC 9(57)V99" => "NUMBER",
                "PIC S9(57)V99" => "NUMBER",
                "PIC 9(59)V99" => "NUMBER",
                "PIC S9(59)V99" => "NUMBER",
                "PIC 9(61)V99" => "NUMBER",
                "PIC S9(61)V99" => "NUMBER",
                "PIC 9(63)V99" => "NUMBER",
                "PIC S9(63)V99" => "NUMBER",
                "PIC 9(65)V99" => "NUMBER",
                "PIC S9(65)V99" => "NUMBER",
                "PIC 9(67)V99" => "NUMBER",
                "PIC S9(67)V99" => "NUMBER",
                "PIC 9(69)V99" => "NUMBER",
                "PIC S9(69)V99" => "NUMBER",
                "PIC 9(71)V99" => "NUMBER",
                "PIC S9(71)V99" => "NUMBER",
                "PIC 9(73)V99" => "NUMBER",
                "PIC S9(73)V99" => "NUMBER",
                "PIC 9(75)V99" => "NUMBER",
                "PIC S9(75)V99" => "NUMBER",
                "PIC 9(77)V99" => "NUMBER",
                "PIC S9(77)V99" => "NUMBER",
                "PIC 9(79)V99" => "NUMBER",
                "PIC S9(79)V99" => "NUMBER",
                "PIC 9(81)V99" => "NUMBER",
                "PIC S9(81)V99" => "NUMBER",
                "PIC 9(83)V99" => "NUMBER",
                "PIC S9(83)V99" => "NUMBER",
                "PIC 9(85)V99" => "NUMBER",
                "PIC S9(85)V99" => "NUMBER",
                "PIC 9(87)V99" => "NUMBER",
                "PIC S9(87)V99" => "NUMBER",
                "PIC 9(89)V99" => "NUMBER",
                "PIC S9(89)V99" => "NUMBER",
                "PIC 9(91)V99" => "NUMBER",
                "PIC S9(91)V99" => "NUMBER",
                "PIC 9(93)V99" => "NUMBER",
                "PIC S9(93)V99" => "NUMBER",
                "PIC 9(95)V99" => "NUMBER",
                "PIC S9(95)V99" => "NUMBER",
                "PIC 9(97)V99" => "NUMBER",
                "PIC S9(97)V99" => "NUMBER",
                "PIC 9(99)V99" => "NUMBER",
                "PIC S9(99)V99" => "NUMBER",
                _ => "CHAR"
            };
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
        /// Creates a deep copy of the FieldDefinition
        /// </summary>
        public FieldDefinition Clone()
        {
            return new FieldDefinition
            {
                FieldName = this.FieldName,
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
    }
} 