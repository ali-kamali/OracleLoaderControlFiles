using ControlFileGenerator.WinForms.Models;
using System.Text;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Service for handling advanced SQL transformations in Oracle SQL*Loader control files
    /// </summary>
    public class AdvancedTransformationService
    {
        /// <summary>
        /// Generates complex SQL transformations based on transformation type and parameters
        /// </summary>
        public string GenerateAdvancedTransformation(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformType))
            {
                return GenerateBasicTransform(field);
            }

            return field.TransformType.ToUpper() switch
            {
                "CASE" => GenerateCaseTransform(field),
                "DECODE" => GenerateDecodeTransform(field),
                "FUNCTION" => GenerateFunctionTransform(field),
                "MATHEMATICAL" => GenerateMathematicalTransform(field),
                "STRING" => GenerateStringTransform(field),
                "DATE" => GenerateDateTransform(field),
                "CONDITIONAL" => GenerateConditionalTransform(field),
                "AGGREGATE" => GenerateAggregateTransform(field),
                "TYPE_CONVERSION" => GenerateTypeConversionTransform(field),
                "CUSTOM" => GenerateCustomTransform(field),
                _ => GenerateBasicTransform(field)
            };
        }

        /// <summary>
        /// Generates a CASE transformation
        /// </summary>
        private string GenerateCaseTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var caseBuilder = new StringBuilder("CASE ");
            var parameters = ParseTransformParameters(field.TransformParameters);

            foreach (var param in parameters)
            {
                if (param.StartsWith("WHEN "))
                {
                    caseBuilder.Append(param).Append(" ");
                }
                else if (param.StartsWith("THEN "))
                {
                    caseBuilder.Append(param).Append(" ");
                }
                else if (param.StartsWith("ELSE "))
                {
                    caseBuilder.Append(param);
                }
            }

            caseBuilder.Append(" END");
            return caseBuilder.ToString();
        }

        /// <summary>
        /// Generates a DECODE transformation
        /// </summary>
        private string GenerateDecodeTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var decodeBuilder = new StringBuilder($"DECODE(:{field.FieldName}");
            var parameters = ParseTransformParameters(field.TransformParameters);

            for (int i = 0; i < parameters.Length; i += 2)
            {
                if (i + 1 < parameters.Length)
                {
                    decodeBuilder.Append($", '{parameters[i]}', '{parameters[i + 1]}'");
                }
                else
                {
                    // Last parameter is the default value
                    decodeBuilder.Append($", '{parameters[i]}'");
                }
            }

            decodeBuilder.Append(")");
            return decodeBuilder.ToString();
        }

        /// <summary>
        /// Generates a function transformation
        /// </summary>
        private string GenerateFunctionTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var parameters = ParseTransformParameters(field.TransformParameters);
            var functionName = parameters.Length > 0 ? parameters[0] : "UPPER";
            var functionParams = parameters.Length > 1 ? parameters[1] : $":{field.FieldName}";

            return $"{functionName}({functionParams})";
        }

        /// <summary>
        /// Generates mathematical transformations
        /// </summary>
        private string GenerateMathematicalTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return $":{field.FieldName}";
            }

            var parameters = ParseTransformParameters(field.TransformParameters);
            var operation = parameters.Length > 0 ? parameters[0] : "*";
            var operand = parameters.Length > 1 ? parameters[1] : "1";

            return $":{field.FieldName} {operation} {operand}";
        }

        /// <summary>
        /// Generates string transformations
        /// </summary>
        private string GenerateStringTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var parameters = ParseTransformParameters(field.TransformParameters);
            var operation = parameters.Length > 0 ? parameters[0] : "UPPER";

            return operation.ToUpper() switch
            {
                "UPPER" => $"UPPER(:{field.FieldName})",
                "LOWER" => $"LOWER(:{field.FieldName})",
                "TRIM" => $"TRIM(:{field.FieldName})",
                "LTRIM" => $"LTRIM(:{field.FieldName})",
                "RTRIM" => $"RTRIM(:{field.FieldName})",
                "SUBSTR" => GenerateSubstrTransform(field, parameters),
                "REPLACE" => GenerateReplaceTransform(field, parameters),
                "CONCAT" => GenerateConcatTransform(field, parameters),
                "LENGTH" => $"LENGTH(:{field.FieldName})",
                "INSTR" => GenerateInstrTransform(field, parameters),
                "LPAD" => GenerateLpadTransform(field, parameters),
                "RPAD" => GenerateRpadTransform(field, parameters),
                "REGEXP_REPLACE" => GenerateRegexpReplaceTransform(field, parameters),
                "REGEXP_SUBSTR" => GenerateRegexpSubstrTransform(field, parameters),
                _ => GenerateBasicTransform(field)
            };
        }

        /// <summary>
        /// Generates date transformations
        /// </summary>
        private string GenerateDateTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var parameters = ParseTransformParameters(field.TransformParameters);
            var operation = parameters.Length > 0 ? parameters[0] : "TO_DATE";
            var format = parameters.Length > 1 ? parameters[1] : "YYYY-MM-DD";

            return operation.ToUpper() switch
            {
                "TO_DATE" => $"TO_DATE(:{field.FieldName}, '{format}')",
                "TO_TIMESTAMP" => $"TO_TIMESTAMP(:{field.FieldName}, '{format}')",
                "TO_CHAR" => $"TO_CHAR(:{field.FieldName}, '{format}')",
                "ADD_MONTHS" => GenerateAddMonthsTransform(field, parameters),
                "MONTHS_BETWEEN" => GenerateMonthsBetweenTransform(field, parameters),
                "NEXT_DAY" => GenerateNextDayTransform(field, parameters),
                "LAST_DAY" => $"LAST_DAY(:{field.FieldName})",
                "TRUNC" => GenerateTruncDateTransform(field, parameters),
                "ROUND" => GenerateRoundDateTransform(field, parameters),
                "EXTRACT" => GenerateExtractTransform(field, parameters),
                "SYSDATE" => "SYSDATE",
                "SYSTIMESTAMP" => "SYSTIMESTAMP",
                _ => GenerateBasicTransform(field)
            };
        }

        /// <summary>
        /// Generates conditional transformations
        /// </summary>
        private string GenerateConditionalTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var parameters = ParseTransformParameters(field.TransformParameters);
            var condition = parameters.Length > 0 ? parameters[0] : "IS NOT NULL";
            var trueValue = parameters.Length > 1 ? parameters[1] : $":{field.FieldName}";
            var falseValue = parameters.Length > 2 ? parameters[2] : "NULL";

            return $"CASE WHEN :{field.FieldName} {condition} THEN {trueValue} ELSE {falseValue} END";
        }

        /// <summary>
        /// Generates aggregate transformations
        /// </summary>
        private string GenerateAggregateTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var parameters = ParseTransformParameters(field.TransformParameters);
            var function = parameters.Length > 0 ? parameters[0] : "SUM";

            return $"{function}(:{field.FieldName})";
        }

        /// <summary>
        /// Generates type conversion transformations
        /// </summary>
        private string GenerateTypeConversionTransform(FieldDefinition field)
        {
            if (string.IsNullOrEmpty(field.TransformParameters))
            {
                return GenerateBasicTransform(field);
            }

            var parameters = ParseTransformParameters(field.TransformParameters);
            var targetType = parameters.Length > 0 ? parameters[0] : "VARCHAR2";
            var format = parameters.Length > 1 ? parameters[1] : "";

            return targetType.ToUpper() switch
            {
                "TO_NUMBER" => $"TO_NUMBER(:{field.FieldName}, '{format}')",
                "TO_CHAR" => $"TO_CHAR(:{field.FieldName}, '{format}')",
                "TO_DATE" => $"TO_DATE(:{field.FieldName}, '{format}')",
                "TO_TIMESTAMP" => $"TO_TIMESTAMP(:{field.FieldName}, '{format}')",
                "CAST" => $"CAST(:{field.FieldName} AS {targetType})",
                "CONVERT" => $"CONVERT(:{field.FieldName}, '{format}')",
                _ => GenerateBasicTransform(field)
            };
        }

        /// <summary>
        /// Generates custom transformations
        /// </summary>
        private string GenerateCustomTransform(FieldDefinition field)
        {
            if (!string.IsNullOrEmpty(field.TransformParameters))
            {
                return field.TransformParameters;
            }

            return GenerateBasicTransform(field);
        }

        /// <summary>
        /// Generates basic transformation
        /// </summary>
        private string GenerateBasicTransform(FieldDefinition field)
        {
            if (!string.IsNullOrEmpty(field.Transform))
            {
                return field.Transform;
            }

            return $":{field.FieldName}";
        }

        #region String Transformation Helpers

        private string GenerateSubstrTransform(FieldDefinition field, string[] parameters)
        {
            var startPos = parameters.Length > 1 ? parameters[1] : "1";
            var length = parameters.Length > 2 ? parameters[2] : "";

            return length.Length > 0 
                ? $"SUBSTR(:{field.FieldName}, {startPos}, {length})"
                : $"SUBSTR(:{field.FieldName}, {startPos})";
        }

        private string GenerateReplaceTransform(FieldDefinition field, string[] parameters)
        {
            var searchStr = parameters.Length > 1 ? parameters[1] : " ";
            var replaceStr = parameters.Length > 2 ? parameters[2] : "";

            return $"REPLACE(:{field.FieldName}, '{searchStr}', '{replaceStr}')";
        }

        private string GenerateConcatTransform(FieldDefinition field, string[] parameters)
        {
            var concatStr = parameters.Length > 1 ? parameters[1] : "";

            return concatStr.Length > 0 
                ? $"CONCAT(:{field.FieldName}, '{concatStr}')"
                : $":{field.FieldName}";
        }

        private string GenerateInstrTransform(FieldDefinition field, string[] parameters)
        {
            var searchStr = parameters.Length > 1 ? parameters[1] : " ";
            var startPos = parameters.Length > 2 ? parameters[2] : "1";
            var occurrence = parameters.Length > 3 ? parameters[3] : "1";

            return $"INSTR(:{field.FieldName}, '{searchStr}', {startPos}, {occurrence})";
        }

        private string GenerateLpadTransform(FieldDefinition field, string[] parameters)
        {
            var length = parameters.Length > 1 ? parameters[1] : "10";
            var padChar = parameters.Length > 2 ? parameters[2] : " ";

            return $"LPAD(:{field.FieldName}, {length}, '{padChar}')";
        }

        private string GenerateRpadTransform(FieldDefinition field, string[] parameters)
        {
            var length = parameters.Length > 1 ? parameters[1] : "10";
            var padChar = parameters.Length > 2 ? parameters[2] : " ";

            return $"RPAD(:{field.FieldName}, {length}, '{padChar}')";
        }

        private string GenerateRegexpReplaceTransform(FieldDefinition field, string[] parameters)
        {
            var pattern = parameters.Length > 1 ? parameters[1] : " ";
            var replaceStr = parameters.Length > 2 ? parameters[2] : "";
            var startPos = parameters.Length > 3 ? parameters[3] : "1";
            var occurrence = parameters.Length > 4 ? parameters[4] : "0";

            return $"REGEXP_REPLACE(:{field.FieldName}, '{pattern}', '{replaceStr}', {startPos}, {occurrence})";
        }

        private string GenerateRegexpSubstrTransform(FieldDefinition field, string[] parameters)
        {
            var pattern = parameters.Length > 1 ? parameters[1] : ".*";
            var startPos = parameters.Length > 2 ? parameters[2] : "1";
            var occurrence = parameters.Length > 3 ? parameters[3] : "1";

            return $"REGEXP_SUBSTR(:{field.FieldName}, '{pattern}', {startPos}, {occurrence})";
        }

        #endregion

        #region Date Transformation Helpers

        private string GenerateAddMonthsTransform(FieldDefinition field, string[] parameters)
        {
            var months = parameters.Length > 1 ? parameters[1] : "1";

            return $"ADD_MONTHS(:{field.FieldName}, {months})";
        }

        private string GenerateMonthsBetweenTransform(FieldDefinition field, string[] parameters)
        {
            var endDate = parameters.Length > 1 ? parameters[1] : "SYSDATE";

            return $"MONTHS_BETWEEN(:{field.FieldName}, {endDate})";
        }

        private string GenerateNextDayTransform(FieldDefinition field, string[] parameters)
        {
            var dayOfWeek = parameters.Length > 1 ? parameters[1] : "MONDAY";

            return $"NEXT_DAY(:{field.FieldName}, '{dayOfWeek}')";
        }

        private string GenerateTruncDateTransform(FieldDefinition field, string[] parameters)
        {
            var format = parameters.Length > 1 ? parameters[1] : "DD";

            return $"TRUNC(:{field.FieldName}, '{format}')";
        }

        private string GenerateRoundDateTransform(FieldDefinition field, string[] parameters)
        {
            var format = parameters.Length > 1 ? parameters[1] : "DD";

            return $"ROUND(:{field.FieldName}, '{format}')";
        }

        private string GenerateExtractTransform(FieldDefinition field, string[] parameters)
        {
            var component = parameters.Length > 1 ? parameters[1] : "YEAR";

            return $"EXTRACT({component} FROM :{field.FieldName})";
        }

        #endregion

        /// <summary>
        /// Parses transformation parameters
        /// </summary>
        private string[] ParseTransformParameters(string parameters)
        {
            return parameters.Split(',', StringSplitOptions.RemoveEmptyEntries)
                           .Select(p => p.Trim())
                           .ToArray();
        }

        /// <summary>
        /// Validates transformation parameters
        /// </summary>
        public List<string> ValidateTransformation(FieldDefinition field)
        {
            var errors = new List<string>();

            if (string.IsNullOrEmpty(field.TransformType))
            {
                return errors; // No transformation type specified is valid
            }

            var validTypes = new[] { "CASE", "DECODE", "FUNCTION", "MATHEMATICAL", "STRING", "DATE", "CONDITIONAL", "AGGREGATE", "TYPE_CONVERSION", "CUSTOM" };
            
            if (!validTypes.Contains(field.TransformType.ToUpper()))
            {
                errors.Add($"Invalid transformation type: {field.TransformType}");
            }

            // Validate specific transformation types
            switch (field.TransformType.ToUpper())
            {
                case "CASE":
                    errors.AddRange(ValidateCaseTransform(field));
                    break;
                case "DECODE":
                    errors.AddRange(ValidateDecodeTransform(field));
                    break;
                case "STRING":
                    errors.AddRange(ValidateStringTransform(field));
                    break;
                case "DATE":
                    errors.AddRange(ValidateDateTransform(field));
                    break;
                case "MATHEMATICAL":
                    errors.AddRange(ValidateMathematicalTransform(field));
                    break;
            }

            return errors;
        }

        private List<string> ValidateCaseTransform(FieldDefinition field)
        {
            var errors = new List<string>();

            if (!string.IsNullOrEmpty(field.TransformParameters))
            {
                var parameters = ParseTransformParameters(field.TransformParameters);
                var whenCount = parameters.Count(p => p.StartsWith("WHEN "));
                var thenCount = parameters.Count(p => p.StartsWith("THEN "));
                var elseCount = parameters.Count(p => p.StartsWith("ELSE "));

                if (whenCount != thenCount)
                {
                    errors.Add("CASE transformation: Number of WHEN clauses must match number of THEN clauses");
                }

                if (elseCount > 1)
                {
                    errors.Add("CASE transformation: Only one ELSE clause is allowed");
                }
            }

            return errors;
        }

        private List<string> ValidateDecodeTransform(FieldDefinition field)
        {
            var errors = new List<string>();

            if (!string.IsNullOrEmpty(field.TransformParameters))
            {
                var parameters = ParseTransformParameters(field.TransformParameters);
                
                if (parameters.Length < 2)
                {
                    errors.Add("DECODE transformation: At least one value-result pair is required");
                }
            }

            return errors;
        }

        private List<string> ValidateStringTransform(FieldDefinition field)
        {
            var errors = new List<string>();

            if (!string.IsNullOrEmpty(field.TransformParameters))
            {
                var parameters = ParseTransformParameters(field.TransformParameters);
                var operation = parameters.Length > 0 ? parameters[0] : "";

                var validOperations = new[] { "UPPER", "LOWER", "TRIM", "LTRIM", "RTRIM", "SUBSTR", "REPLACE", "CONCAT", "LENGTH", "INSTR", "LPAD", "RPAD", "REGEXP_REPLACE", "REGEXP_SUBSTR" };

                if (!validOperations.Contains(operation.ToUpper()))
                {
                    errors.Add($"Invalid string operation: {operation}");
                }
            }

            return errors;
        }

        private List<string> ValidateDateTransform(FieldDefinition field)
        {
            var errors = new List<string>();

            if (!string.IsNullOrEmpty(field.TransformParameters))
            {
                var parameters = ParseTransformParameters(field.TransformParameters);
                var operation = parameters.Length > 0 ? parameters[0] : "";

                var validOperations = new[] { "TO_DATE", "TO_TIMESTAMP", "TO_CHAR", "ADD_MONTHS", "MONTHS_BETWEEN", "NEXT_DAY", "LAST_DAY", "TRUNC", "ROUND", "EXTRACT", "SYSDATE", "SYSTIMESTAMP" };

                if (!validOperations.Contains(operation.ToUpper()))
                {
                    errors.Add($"Invalid date operation: {operation}");
                }
            }

            return errors;
        }

        private List<string> ValidateMathematicalTransform(FieldDefinition field)
        {
            var errors = new List<string>();

            if (!string.IsNullOrEmpty(field.TransformParameters))
            {
                var parameters = ParseTransformParameters(field.TransformParameters);
                var operation = parameters.Length > 0 ? parameters[0] : "";

                var validOperations = new[] { "+", "-", "*", "/", "MOD", "POWER", "SQRT", "ABS", "ROUND", "TRUNC", "CEIL", "FLOOR" };

                if (!validOperations.Contains(operation))
                {
                    errors.Add($"Invalid mathematical operation: {operation}");
                }
            }

            return errors;
        }

        /// <summary>
        /// Generates transformation examples
        /// </summary>
        public List<FieldDefinition> GenerateTransformationExamples()
        {
            return new List<FieldDefinition>
            {
                // String Transformations
                new FieldDefinition
                {
                    FieldName = "name",
                    TransformType = "STRING",
                    TransformParameters = "UPPER",
                    SqlType = "VARCHAR2",
                    Length = 50
                },
                new FieldDefinition
                {
                    FieldName = "description",
                    TransformType = "STRING",
                    TransformParameters = "SUBSTR,1,100",
                    SqlType = "VARCHAR2",
                    Length = 100
                },
                new FieldDefinition
                {
                    FieldName = "text_field",
                    TransformType = "STRING",
                    TransformParameters = "REPLACE,old,new",
                    SqlType = "VARCHAR2",
                    Length = 200
                },

                // Date Transformations
                new FieldDefinition
                {
                    FieldName = "date_field",
                    TransformType = "DATE",
                    TransformParameters = "TO_DATE,YYYY-MM-DD",
                    SqlType = "DATE"
                },
                new FieldDefinition
                {
                    FieldName = "timestamp_field",
                    TransformType = "DATE",
                    TransformParameters = "TO_TIMESTAMP,YYYY-MM-DD HH24:MI:SS",
                    SqlType = "TIMESTAMP"
                },

                // Mathematical Transformations
                new FieldDefinition
                {
                    FieldName = "amount",
                    TransformType = "MATHEMATICAL",
                    TransformParameters = "*,100",
                    SqlType = "NUMBER",
                    Precision = 10,
                    Scale = 2
                },
                new FieldDefinition
                {
                    FieldName = "quantity",
                    TransformType = "MATHEMATICAL",
                    TransformParameters = "ROUND,2",
                    SqlType = "NUMBER",
                    Precision = 5
                },

                // Conditional Transformations
                new FieldDefinition
                {
                    FieldName = "status",
                    TransformType = "CONDITIONAL",
                    TransformParameters = "IS NOT NULL,ACTIVE,INACTIVE",
                    SqlType = "VARCHAR2",
                    Length = 10
                },

                // Type Conversion Transformations
                new FieldDefinition
                {
                    FieldName = "numeric_string",
                    TransformType = "TYPE_CONVERSION",
                    TransformParameters = "TO_NUMBER,999999.99",
                    SqlType = "NUMBER",
                    Precision = 8,
                    Scale = 2
                }
            };
        }
    }
} 