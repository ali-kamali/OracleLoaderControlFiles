using ControlFileGenerator.WinForms.Models;
using System.Text.RegularExpressions;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Provides intelligent auto-completion suggestions for field definitions based on Oracle/SQL data types
    /// </summary>
    public static class SmartAutoCompletionService
    {
        /// <summary>
        /// Gets smart suggestions for transforms based on SQL type
        /// </summary>
        public static List<string> GetTransformSuggestions(string sqlType, string fieldName)
        {
            var suggestions = new List<string>();
            
            if (string.IsNullOrWhiteSpace(sqlType))
                return suggestions;

            var normalizedType = sqlType.ToUpper().Trim();
            var baseType = GetBaseType(normalizedType);

            switch (baseType)
            {
                case "CHAR":
                case "VARCHAR2":
                    suggestions.AddRange(GetStringTransformSuggestions(fieldName));
                    break;
                    
                case "NUMBER":
                    suggestions.AddRange(GetNumericTransformSuggestions(fieldName, normalizedType));
                    break;
                    
                case "DATE":
                    suggestions.AddRange(GetDateTransformSuggestions(fieldName));
                    break;
                    
                case "TIMESTAMP":
                    suggestions.AddRange(GetTimestampTransformSuggestions(fieldName));
                    break;
                    
                case "CLOB":
                    suggestions.AddRange(GetClobTransformSuggestions(fieldName));
                    break;
                    
                case "BLOB":
                    suggestions.AddRange(GetBlobTransformSuggestions(fieldName));
                    break;
            }

            return suggestions.Distinct().ToList();
        }

        /// <summary>
        /// Gets smart suggestions for null values based on SQL type
        /// </summary>
        public static List<string> GetNullValueSuggestions(string sqlType, string fieldName)
        {
            var suggestions = new List<string>();
            
            if (string.IsNullOrWhiteSpace(sqlType))
                return suggestions;

            var normalizedType = sqlType.ToUpper().Trim();
            var baseType = GetBaseType(normalizedType);

            switch (baseType)
            {
                case "CHAR":
                case "VARCHAR2":
                    suggestions.AddRange(new[]
                    {
                        "BLANKS",
                        "''",
                        "'NULL'",
                        "'N/A'",
                        "'UNKNOWN'",
                        IntelligentNullValueProcessor.SmartNullPatterns.EMPTY_OR_WHITESPACE,
                        IntelligentNullValueProcessor.SmartNullPatterns.TRIM_IF_NOT_EMPTY,
                        IntelligentNullValueProcessor.SmartNullPatterns.EMPTY_OR_NULL
                    });
                    break;
                    
                case "NUMBER":
                    suggestions.AddRange(new[]
                    {
                        "0",
                        "-1",
                        "999999",
                        "BLANKS",
                        "'NULL'",
                        "'N/A'"
                    });
                    break;
                    
                case "DATE":
                    suggestions.AddRange(new[]
                    {
                        "BLANKS",
                        "'NULL'",
                        "'1900-01-01'",
                        "'9999-12-31'",
                        "'00000000'",
                        "'000000'"
                    });
                    break;
                    
                case "TIMESTAMP":
                    suggestions.AddRange(new[]
                    {
                        "BLANKS",
                        "'NULL'",
                        "'1900-01-01 00:00:00'",
                        "'9999-12-31 23:59:59'"
                    });
                    break;
            }

            return suggestions.Distinct().ToList();
        }

        /// <summary>
        /// Gets smart suggestions for default values based on SQL type
        /// </summary>
        public static List<string> GetDefaultValueSuggestions(string sqlType, string fieldName)
        {
            var suggestions = new List<string>();
            
            if (string.IsNullOrWhiteSpace(sqlType))
                return suggestions;

            var normalizedType = sqlType.ToUpper().Trim();
            var baseType = GetBaseType(normalizedType);

            switch (baseType)
            {
                case "CHAR":
                case "VARCHAR2":
                    suggestions.AddRange(new[]
                    {
                        "' '",
                        "'N/A'",
                        "'UNKNOWN'",
                        "'DEFAULT'",
                        "SYSDATE",
                        "USER"
                    });
                    break;
                    
                case "NUMBER":
                    suggestions.AddRange(new[]
                    {
                        "0",
                        "1",
                        "-1",
                        "999999",
                        "SYSDATE",
                        "USER"
                    });
                    break;
                    
                case "DATE":
                    suggestions.AddRange(new[]
                    {
                        "SYSDATE",
                        "'1900-01-01'",
                        "'9999-12-31'",
                        "TRUNC(SYSDATE)",
                        "SYSDATE - 1"
                    });
                    break;
                    
                case "TIMESTAMP":
                    suggestions.AddRange(new[]
                    {
                        "SYSTIMESTAMP",
                        "SYSDATE",
                        "'1900-01-01 00:00:00'",
                        "'9999-12-31 23:59:59'"
                    });
                    break;
            }

            return suggestions.Distinct().ToList();
        }

        /// <summary>
        /// Gets smart suggestions for data formats based on SQL type
        /// </summary>
        public static List<string> GetDataFormatSuggestions(string sqlType, string fieldName)
        {
            var suggestions = new List<string>();
            
            if (string.IsNullOrWhiteSpace(sqlType))
                return suggestions;

            var normalizedType = sqlType.ToUpper().Trim();
            var baseType = GetBaseType(normalizedType);

            switch (baseType)
            {
                case "DATE":
                    suggestions.AddRange(new[]
                    {
                        "YYYY-MM-DD",
                        "MM/DD/YYYY",
                        "DD/MM/YYYY",
                        "YYYYMMDD",
                        "MMDDYYYY",
                        "DDMMYYYY",
                        "YYYY-DDD", // Julian date
                        "DD-MON-YYYY",
                        "MON-DD-YYYY"
                    });
                    break;
                    
                case "TIMESTAMP":
                    suggestions.AddRange(new[]
                    {
                        "YYYY-MM-DD HH24:MI:SS",
                        "MM/DD/YYYY HH24:MI:SS",
                        "DD/MM/YYYY HH24:MI:SS",
                        "YYYYMMDDHH24MISS",
                        "YYYY-MM-DD HH24:MI:SS.FF",
                        "YYYY-MM-DD HH24:MI:SS.FF3",
                        "YYYY-MM-DD HH24:MI:SS.FF6",
                        "YYYY-MM-DD HH24:MI:SS.FF9"
                    });
                    break;
                    
                case "NUMBER":
                    suggestions.AddRange(new[]
                    {
                        "999999999.99",
                        "999999999",
                        "999.99",
                        "999999999999999",
                        "FM999999999.99",
                        "FM999999999",
                        "L999999999.99",
                        "$999999999.99"
                    });
                    break;
            }

            return suggestions.Distinct().ToList();
        }

        /// <summary>
        /// Gets smart suggestions for field terminators based on context
        /// </summary>
        public static List<string> GetFieldTerminatorSuggestions(bool isFixedWidthMode)
        {
            if (isFixedWidthMode)
            {
                return new List<string>
                {
                    "WHITESPACE",
                    "EOF"
                };
            }
            else
            {
                return new List<string>
                {
                    ",",
                    ";",
                    "|",
                    "\t",
                    "~",
                    "WHITESPACE",
                    "EOF"
                };
            }
        }

        /// <summary>
        /// Gets smart suggestions for field enclosure based on context
        /// </summary>
        public static List<string> GetFieldEnclosureSuggestions()
        {
            return new List<string>
            {
                "\"",
                "'",
                "`",
                "|",
                "~"
            };
        }

        /// <summary>
        /// Gets smart suggestions for validation rules based on SQL type
        /// </summary>
        public static List<string> GetValidationRuleSuggestions(string sqlType, string fieldName)
        {
            var suggestions = new List<string>();
            
            if (string.IsNullOrWhiteSpace(sqlType))
                return suggestions;

            var normalizedType = sqlType.ToUpper().Trim();
            var baseType = GetBaseType(normalizedType);

            switch (baseType)
            {
                case "CHAR":
                case "VARCHAR2":
                    suggestions.AddRange(new[]
                    {
                        $"LENGTH({fieldName}) > 0",
                        $"LENGTH({fieldName}) <= 255",
                        $"REGEXP_LIKE({fieldName}, '^[A-Za-z0-9_]+$')",
                        $"REGEXP_LIKE({fieldName}, '^[A-Za-z]+$')",
                        $"REGEXP_LIKE({fieldName}, '^[0-9]+$')",
                        $"{fieldName} IS NOT NULL",
                        $"UPPER({fieldName}) IN ('Y', 'N')",
                        $"UPPER({fieldName}) IN ('YES', 'NO')"
                    });
                    break;
                    
                case "NUMBER":
                    suggestions.AddRange(new[]
                    {
                        $"{fieldName} > 0",
                        $"{fieldName} >= 0",
                        $"{fieldName} BETWEEN 0 AND 999999",
                        $"{fieldName} IS NOT NULL",
                        $"MOD({fieldName}, 1) = 0", // Integer check
                        $"LENGTH(TO_CHAR({fieldName})) <= 10"
                    });
                    break;
                    
                case "DATE":
                    suggestions.AddRange(new[]
                    {
                        $"{fieldName} IS NOT NULL",
                        $"{fieldName} >= TO_DATE('1900-01-01', 'YYYY-MM-DD')",
                        $"{fieldName} <= TO_DATE('9999-12-31', 'YYYY-MM-DD')",
                        $"EXTRACT(YEAR FROM {fieldName}) BETWEEN 1900 AND 9999"
                    });
                    break;
            }

            return suggestions.Distinct().ToList();
        }

        /// <summary>
        /// Gets smart suggestions for transform parameters based on transform type
        /// </summary>
        public static List<string> GetTransformParameterSuggestions(string transformType, string sqlType)
        {
            var suggestions = new List<string>();
            
            if (string.IsNullOrWhiteSpace(transformType))
                return suggestions;

            var normalizedTransformType = transformType.ToUpper().Trim();
            var normalizedSqlType = sqlType.ToUpper().Trim();

            switch (normalizedTransformType)
            {
                case "CASE":
                    if (normalizedSqlType.StartsWith("CHAR") || normalizedSqlType.StartsWith("VARCHAR"))
                    {
                        suggestions.AddRange(new[]
                        {
                            "CASE WHEN :field = 'Y' THEN 'YES' WHEN :field = 'N' THEN 'NO' ELSE :field END",
                            "CASE WHEN :field IS NULL THEN 'UNKNOWN' ELSE UPPER(:field) END",
                            "CASE WHEN LENGTH(:field) = 0 THEN 'EMPTY' ELSE :field END"
                        });
                    }
                    else if (normalizedSqlType.StartsWith("NUMBER"))
                    {
                        suggestions.AddRange(new[]
                        {
                            "CASE WHEN :field > 0 THEN 'POSITIVE' WHEN :field < 0 THEN 'NEGATIVE' ELSE 'ZERO' END",
                            "CASE WHEN :field IS NULL THEN 0 ELSE :field END",
                            "CASE WHEN :field BETWEEN 1 AND 100 THEN 'LOW' WHEN :field BETWEEN 101 AND 1000 THEN 'MEDIUM' ELSE 'HIGH' END"
                        });
                    }
                    break;
                    
                case "DECODE":
                    if (normalizedSqlType.StartsWith("CHAR") || normalizedSqlType.StartsWith("VARCHAR"))
                    {
                        suggestions.AddRange(new[]
                        {
                            "DECODE(:field, 'Y', 'YES', 'N', 'NO', :field)",
                            "DECODE(:field, NULL, 'UNKNOWN', :field)",
                            "DECODE(LENGTH(:field), 0, 'EMPTY', :field)"
                        });
                    }
                    break;
                    
                case "FUNCTION":
                    if (normalizedSqlType.StartsWith("CHAR") || normalizedSqlType.StartsWith("VARCHAR"))
                    {
                        suggestions.AddRange(new[]
                        {
                            "UPPER(:field)",
                            "LOWER(:field)",
                            "TRIM(:field)",
                            "LTRIM(:field)",
                            "RTRIM(:field)",
                            "SUBSTR(:field, 1, 10)",
                            "REPLACE(:field, ' ', '_')",
                            "REGEXP_REPLACE(:field, '[^A-Za-z0-9]', '')"
                        });
                    }
                    else if (normalizedSqlType.StartsWith("NUMBER"))
                    {
                        suggestions.AddRange(new[]
                        {
                            "ROUND(:field, 2)",
                            "TRUNC(:field)",
                            "ABS(:field)",
                            "CEIL(:field)",
                            "FLOOR(:field)",
                            "MOD(:field, 100)"
                        });
                    }
                    else if (normalizedSqlType.StartsWith("DATE"))
                    {
                        suggestions.AddRange(new[]
                        {
                            "TRUNC(:field)",
                            "TO_CHAR(:field, 'YYYY-MM-DD')",
                            "TO_CHAR(:field, 'MM/DD/YYYY')",
                            "EXTRACT(YEAR FROM :field)",
                            "EXTRACT(MONTH FROM :field)",
                            "EXTRACT(DAY FROM :field)"
                        });
                    }
                    break;
            }

            return suggestions.Distinct().ToList();
        }

        /// <summary>
        /// Gets smart suggestions for field character sets
        /// </summary>
        public static List<string> GetCharacterSetSuggestions()
        {
            return new List<string>
            {
                "UTF8",
                "AL32UTF8",
                "WE8ISO8859P1",
                "WE8MSWIN1252",
                "US7ASCII",
                "ZHS16GBK",
                "JA16SJIS",
                "KO16MSWIN949"
            };
        }

        /// <summary>
        /// Gets smart suggestions for binary formats
        /// </summary>
        public static List<string> GetBinaryFormatSuggestions()
        {
            return new List<string>
            {
                "BIG_ENDIAN",
                "LITTLE_ENDIAN",
                "SIGNED",
                "UNSIGNED",
                "SIGNED_BYTE",
                "UNSIGNED_BYTE",
                "SIGNED_SHORT",
                "UNSIGNED_SHORT",
                "SIGNED_INT",
                "UNSIGNED_INT",
                "SIGNED_LONG",
                "UNSIGNED_LONG"
            };
        }

        /// <summary>
        /// Gets smart suggestions for numeric formats
        /// </summary>
        public static List<string> GetNumericFormatSuggestions()
        {
            return new List<string>
            {
                "EXTERNAL",
                "INTERNAL",
                "PACKED_DECIMAL",
                "ZONED_DECIMAL",
                "BINARY",
                "FLOAT",
                "DOUBLE"
            };
        }

        /// <summary>
        /// Gets smart suggestions for time zone formats
        /// </summary>
        public static List<string> GetTimeZoneFormatSuggestions()
        {
            return new List<string>
            {
                "UTC",
                "LOCAL",
                "GMT",
                "EST",
                "PST",
                "CST",
                "MST",
                "+00:00",
                "-05:00",
                "-08:00"
            };
        }

        #region Private Helper Methods

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

        private static List<string> GetStringTransformSuggestions(string fieldName)
        {
            return new List<string>
            {
                $"UPPER(:{fieldName})",
                $"LOWER(:{fieldName})",
                $"TRIM(:{fieldName})",
                $"LTRIM(:{fieldName})",
                $"RTRIM(:{fieldName})",
                $"SUBSTR(:{fieldName}, 1, 50)",
                $"REPLACE(:{fieldName}, ' ', '_')",
                $"REGEXP_REPLACE(:{fieldName}, '[^A-Za-z0-9]', '')",
                $"REGEXP_REPLACE(:{fieldName}, '\\s+', ' ')",
                $"INITCAP(:{fieldName})",
                $"TRANSLATE(:{fieldName}, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')",
                $"CASE WHEN LENGTH(:{fieldName}) = 0 THEN 'EMPTY' ELSE :{fieldName} END",
                $"CASE WHEN :{fieldName} IS NULL THEN 'UNKNOWN' ELSE :{fieldName} END"
            };
        }

        private static List<string> GetNumericTransformSuggestions(string fieldName, string sqlType)
        {
            var suggestions = new List<string>
            {
                $"ROUND(:{fieldName}, 2)",
                $"TRUNC(:{fieldName})",
                $"ABS(:{fieldName})",
                $"CEIL(:{fieldName})",
                $"FLOOR(:{fieldName})",
                $"MOD(:{fieldName}, 100)",
                $"POWER(:{fieldName}, 2)",
                $"SQRT(:{fieldName})",
                $"CASE WHEN :{fieldName} IS NULL THEN 0 ELSE :{fieldName} END"
            };

            // Add precision-specific suggestions
            if (sqlType.Contains("(") && sqlType.Contains(")"))
            {
                var match = Regex.Match(sqlType, @"NUMBER\((\d+)(?:,(\d+))?\)");
                if (match.Success)
                {
                    if (int.TryParse(match.Groups[1].Value, out int precision))
                    {
                        suggestions.Add($"CASE WHEN LENGTH(TO_CHAR(:{fieldName})) > {precision} THEN NULL ELSE :{fieldName} END");
                    }
                }
            }

            return suggestions;
        }

        private static List<string> GetDateTransformSuggestions(string fieldName)
        {
            return new List<string>
            {
                $"TRUNC(:{fieldName})",
                $"TO_CHAR(:{fieldName}, 'YYYY-MM-DD')",
                $"TO_CHAR(:{fieldName}, 'MM/DD/YYYY')",
                $"TO_CHAR(:{fieldName}, 'DD/MM/YYYY')",
                $"EXTRACT(YEAR FROM :{fieldName})",
                $"EXTRACT(MONTH FROM :{fieldName})",
                $"EXTRACT(DAY FROM :{fieldName})",
                $"ADD_MONTHS(:{fieldName}, 0)",
                $"LAST_DAY(:{fieldName})",
                $"NEXT_DAY(:{fieldName}, 'MONDAY')",
                $"CASE WHEN :{fieldName} IS NULL THEN SYSDATE ELSE :{fieldName} END"
            };
        }

        private static List<string> GetTimestampTransformSuggestions(string fieldName)
        {
            return new List<string>
            {
                $"TRUNC(:{fieldName})",
                $"TO_CHAR(:{fieldName}, 'YYYY-MM-DD HH24:MI:SS')",
                $"TO_CHAR(:{fieldName}, 'MM/DD/YYYY HH24:MI:SS')",
                $"EXTRACT(YEAR FROM :{fieldName})",
                $"EXTRACT(MONTH FROM :{fieldName})",
                $"EXTRACT(DAY FROM :{fieldName})",
                $"EXTRACT(HOUR FROM :{fieldName})",
                $"EXTRACT(MINUTE FROM :{fieldName})",
                $"EXTRACT(SECOND FROM :{fieldName})",
                $"CASE WHEN :{fieldName} IS NULL THEN SYSTIMESTAMP ELSE :{fieldName} END"
            };
        }

        private static List<string> GetClobTransformSuggestions(string fieldName)
        {
            return new List<string>
            {
                $"TO_CLOB(:{fieldName})",
                $"DBMS_LOB.SUBSTR(:{fieldName}, 4000, 1)",
                $"CASE WHEN :{fieldName} IS NULL THEN EMPTY_CLOB() ELSE :{fieldName} END",
                $"REGEXP_REPLACE(:{fieldName}, '\\s+', ' ')",
                $"UPPER(:{fieldName})",
                $"LOWER(:{fieldName})"
            };
        }

        private static List<string> GetBlobTransformSuggestions(string fieldName)
        {
            return new List<string>
            {
                $"TO_BLOB(:{fieldName})",
                $"DBMS_LOB.SUBSTR(:{fieldName}, 4000, 1)",
                $"CASE WHEN :{fieldName} IS NULL THEN EMPTY_BLOB() ELSE :{fieldName} END",
                $"UTL_RAW.CAST_TO_RAW(:{fieldName})"
            };
        }

        #endregion
    }
} 