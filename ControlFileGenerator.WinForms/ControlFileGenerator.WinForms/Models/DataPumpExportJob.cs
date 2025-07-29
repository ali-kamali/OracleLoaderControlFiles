using System.ComponentModel;

namespace ControlFileGenerator.WinForms.Models
{
    public class DataPumpExportJob
    {
        [DisplayName("Job ID")]
        public string JobId { get; set; } = string.Empty;

        [DisplayName("Job Name")]
        public string JobName { get; set; } = string.Empty;

        [DisplayName("Description")]
        public string Description { get; set; } = string.Empty;

        [DisplayName("SQL Query")]
        public string SqlQuery { get; set; } = string.Empty;

        [DisplayName("Oracle Directory Object")]
        public string OracleDirectory { get; set; } = string.Empty;

        [DisplayName("Dump File Name")]
        public string DumpFileName { get; set; } = string.Empty;

        [DisplayName("Log File Name")]
        public string LogFileName { get; set; } = string.Empty;

        [DisplayName("Parallel Processes")]
        public int ParallelProcesses { get; set; } = 1;

        [DisplayName("Full Database Export")]
        public bool IsFullExport { get; set; } = false;

        [DisplayName("Schema/Table Name")]
        public string SchemaTableName { get; set; } = string.Empty;

        [DisplayName("Export Type")]
        public ExportType ExportType { get; set; } = ExportType.Schemas;

        [DisplayName("Content Type")]
        public ContentType ContentType { get; set; } = ContentType.All;

        [DisplayName("Compression")]
        public CompressionType CompressionType { get; set; } = CompressionType.None;

        [DisplayName("Encryption")]
        public EncryptionType EncryptionType { get; set; } = EncryptionType.None;

        [DisplayName("Encryption Password")]
        public string EncryptionPassword { get; set; } = string.Empty;

        [DisplayName("Exclude Objects")]
        public string ExcludeObjects { get; set; } = string.Empty;

        [DisplayName("Include Objects")]
        public string IncludeObjects { get; set; } = string.Empty;

        [DisplayName("Estimate Only")]
        public bool EstimateOnly { get; set; } = false;

        [DisplayName("Estimate Method")]
        public EstimateMethod EstimateMethod { get; set; } = EstimateMethod.Blocks;

        [DisplayName("Flashback Time")]
        public string FlashbackTime { get; set; } = string.Empty;

        [DisplayName("Flashback SCN")]
        public string FlashbackScn { get; set; } = string.Empty;

        [DisplayName("Sample Percentage")]
        public int SamplePercentage { get; set; } = 0;

        [DisplayName("Status Interval")]
        public int StatusInterval { get; set; } = 0;

        [DisplayName("Job Name")]
        public string DataPumpJobName { get; set; } = string.Empty;

        [DisplayName("Oracle SID")]
        public string OracleSid { get; set; } = string.Empty;

        [DisplayName("Oracle Home Path")]
        public string OracleHome { get; set; } = string.Empty;

        [DisplayName("Database Username")]
        public string Username { get; set; } = string.Empty;

        [DisplayName("Database Password")]
        public string Password { get; set; } = string.Empty;

        [DisplayName("Database Name")]
        public string DatabaseName { get; set; } = string.Empty;

        [DisplayName("Created Date")]
        public DateTime CreatedDate { get; set; } = DateTime.Now;

        [DisplayName("Last Modified")]
        public DateTime LastModified { get; set; } = DateTime.Now;
    }

    public enum ExportType
    {
        [Description("Schemas")]
        Schemas,
        [Description("Tables")]
        Tables,
        [Description("Full Database")]
        Full,
        [Description("Transportable Tablespaces")]
        TransportableTablespaces,
        [Description("Tablespaces")]
        Tablespaces
    }

    public enum ContentType
    {
        [Description("All")]
        All,
        [Description("Data Only")]
        DataOnly,
        [Description("Metadata Only")]
        MetadataOnly
    }

    public enum CompressionType
    {
        [Description("None")]
        None,
        [Description("Basic")]
        Basic,
        [Description("Low")]
        Low,
        [Description("Medium")]
        Medium,
        [Description("High")]
        High
    }

    public enum EncryptionType
    {
        [Description("None")]
        None,
        [Description("Password")]
        Password,
        [Description("Transparent")]
        Transparent,
        [Description("Dual")]
        Dual
    }

    public enum EstimateMethod
    {
        [Description("Blocks")]
        Blocks,
        [Description("Statistics")]
        Statistics
    }
} 