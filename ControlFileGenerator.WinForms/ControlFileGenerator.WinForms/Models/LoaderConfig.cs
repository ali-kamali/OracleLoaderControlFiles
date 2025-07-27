using System.ComponentModel;

namespace ControlFileGenerator.WinForms.Models
{
    public class LoaderConfig
    {
        [DisplayName("Table Name")]
        public string TableName { get; set; } = string.Empty;

        [DisplayName("Load Mode")]
        public string LoadMode { get; set; } = "APPEND";

        [DisplayName("Input File")]
        public string Infile { get; set; } = string.Empty;

        [DisplayName("Bad File")]
        public string Badfile { get; set; } = string.Empty;

        [DisplayName("Discard File")]
        public string Discardfile { get; set; } = string.Empty;

        [DisplayName("Trailing Null Columns")]
        public bool TrailingNullCols { get; set; } = false;

        [DisplayName("Use Direct Path")]
        public bool UseDirectPath { get; set; } = false;

        [DisplayName("Max Errors")]
        public int MaxErrors { get; set; } = 50;

        [DisplayName("Bind Size")]
        public int BindSize { get; set; } = 1048576;

        [DisplayName("Rows")]
        public int Rows { get; set; } = 50000;

        [DisplayName("Skip Rows")]
        public int SkipRows { get; set; } = 0;

        [DisplayName("Load Rows")]
        public int LoadRows { get; set; } = 0;

        [DisplayName("Character Set")]
        public string CharacterSet { get; set; } = "UTF8";

        [DisplayName("Global File Encoding")]
        public string GlobalFileEncoding { get; set; } = "UTF8";

        [DisplayName("Use Specific Partition")]
        public bool UseSpecificPartition { get; set; } = false;

        [DisplayName("Partition Name")]
        public string PartitionName { get; set; } = string.Empty;

        [DisplayName("Field Terminator")]
        public string FieldTerminator { get; set; } = ",";

        [DisplayName("Enclosed By")]
        public string EnclosedBy { get; set; } = "\"";

        [DisplayName("Optionally Enclosed")]
        public bool OptionallyEnclosed { get; set; } = true;

        [DisplayName("Trim Option")]
        public string TrimOption { get; set; } = "LRTRIM";

        [DisplayName("Preserve Blanks")]
        public bool PreserveBlanks { get; set; } = false;

        /// <summary>
        /// Gets the load mode string for the control file
        /// </summary>
        public string GetLoadModeString()
        {
            return LoadMode.ToUpper();
        }

        /// <summary>
        /// Gets the character set specification if different from default
        /// </summary>
        public string GetCharacterSetString()
        {
            if (string.IsNullOrEmpty(CharacterSet) || CharacterSet.Equals("UTF8", StringComparison.OrdinalIgnoreCase))
            {
                return string.Empty;
            }
            return $"CHARACTERSET {CharacterSet}";
        }

        /// <summary>
        /// Gets the global file encoding specification if different from default
        /// </summary>
        public string GetGlobalFileEncodingString()
        {
            if (string.IsNullOrEmpty(GlobalFileEncoding) || GlobalFileEncoding.Equals("UTF8", StringComparison.OrdinalIgnoreCase))
            {
                return string.Empty;
            }
            return $"CHARACTERSET {GlobalFileEncoding}";
        }

        /// <summary>
        /// Gets the partition specification if enabled
        /// </summary>
        public string GetPartitionString()
        {
            if (!UseSpecificPartition || string.IsNullOrWhiteSpace(PartitionName))
            {
                return string.Empty;
            }
            return $"PARTITION ({PartitionName})";
        }

        /// <summary>
        /// Gets the field specification string for delimited files
        /// </summary>
        public string GetFieldSpecificationString()
        {
            var parts = new List<string>();

            if (!string.IsNullOrEmpty(FieldTerminator))
            {
                parts.Add($"TERMINATED BY '{FieldTerminator}'");
            }

            if (!string.IsNullOrEmpty(EnclosedBy))
            {
                var enclosedBy = OptionallyEnclosed ? "OPTIONALLY ENCLOSED BY" : "ENCLOSED BY";
                parts.Add($"{enclosedBy} '{EnclosedBy}'");
            }

            if (!string.IsNullOrEmpty(TrimOption) && TrimOption != "NOTRIM")
            {
                parts.Add(TrimOption);
            }

            if (PreserveBlanks)
            {
                parts.Add("PRESERVE BLANKS");
            }

            return string.Join(" ", parts);
        }

        /// <summary>
        /// Gets the processing options string
        /// </summary>
        public string GetProcessingOptionsString()
        {
            var options = new List<string>();

            if (SkipRows > 0)
            {
                options.Add($"SKIP {SkipRows}");
            }

            if (LoadRows > 0)
            {
                options.Add($"LOAD {LoadRows}");
            }

            if (MaxErrors != 50)
            {
                options.Add($"ERRORS {MaxErrors}");
            }

            if (Rows != 50000)
            {
                options.Add($"ROWS {Rows}");
            }

            if (BindSize != 1048576)
            {
                options.Add($"BINDSIZE {BindSize}");
            }

            if (UseDirectPath)
            {
                options.Add("DIRECT=TRUE");
            }

            return string.Join(Environment.NewLine, options);
        }

        /// <summary>
        /// Gets the file specifications string
        /// </summary>
        public string GetFileSpecificationsString()
        {
            var specs = new List<string>();

            if (!string.IsNullOrEmpty(Infile))
            {
                specs.Add($"INFILE '{Infile}'");
            }

            if (!string.IsNullOrEmpty(Badfile))
            {
                specs.Add($"BADFILE '{Badfile}'");
            }

            if (!string.IsNullOrEmpty(Discardfile))
            {
                specs.Add($"DISCARDFILE '{Discardfile}'");
            }

            return string.Join(Environment.NewLine, specs);
        }

        /// <summary>
        /// Creates a deep copy of the LoaderConfig
        /// </summary>
        public LoaderConfig Clone()
        {
            return new LoaderConfig
            {
                TableName = this.TableName,
                LoadMode = this.LoadMode,
                Infile = this.Infile,
                Badfile = this.Badfile,
                Discardfile = this.Discardfile,
                TrailingNullCols = this.TrailingNullCols,
                UseDirectPath = this.UseDirectPath,
                MaxErrors = this.MaxErrors,
                BindSize = this.BindSize,
                Rows = this.Rows,
                SkipRows = this.SkipRows,
                LoadRows = this.LoadRows,
                CharacterSet = this.CharacterSet,
                GlobalFileEncoding = this.GlobalFileEncoding,
                UseSpecificPartition = this.UseSpecificPartition,
                PartitionName = this.PartitionName,
                FieldTerminator = this.FieldTerminator,
                EnclosedBy = this.EnclosedBy,
                OptionallyEnclosed = this.OptionallyEnclosed,
                TrimOption = this.TrimOption,
                PreserveBlanks = this.PreserveBlanks
            };
        }

        /// <summary>
        /// Validates the configuration
        /// </summary>
        public List<string> Validate()
        {
            var errors = new List<string>();

            if (string.IsNullOrWhiteSpace(TableName))
            {
                errors.Add("Table name is required");
            }

            if (string.IsNullOrWhiteSpace(Infile))
            {
                errors.Add("Input file path is required");
            }

            if (MaxErrors < 0)
            {
                errors.Add("Max errors must be non-negative");
            }

            if (BindSize < 1024)
            {
                errors.Add("Bind size must be at least 1024 bytes");
            }

            if (Rows < 1)
            {
                errors.Add("Rows must be at least 1");
            }

            return errors;
        }
    }
} 