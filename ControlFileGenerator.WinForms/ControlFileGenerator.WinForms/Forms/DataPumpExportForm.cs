using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class DataPumpExportForm : Form
    {
        public DataPumpExportJob ExportJob { get; private set; }

        public DataPumpExportForm(DataPumpExportJob? exportJob = null)
        {
            InitializeComponent();
            ExportJob = exportJob ?? new DataPumpExportJob();
            
            InitializeForm();
            LoadExportJob();
        }

        private void InitializeForm()
        {
            // Initialize Oracle Directory ComboBox with common directory names
            cboOracleDirectory.Items.AddRange(new object[] 
            { 
                "DATA_PUMP_DIR", 
                "EXPORT_DIR", 
                "DUMP_DIR", 
                "BACKUP_DIR",
                "TEMP_DIR"
            });
            cboOracleDirectory.SelectedIndex = 0;

            // Initialize Export Type ComboBox
            cboExportType.Items.AddRange(Enum.GetValues(typeof(ExportType)).Cast<object>().ToArray());
            cboExportType.SelectedIndex = 0;

            // Initialize Content Type ComboBox
            cboContentType.Items.AddRange(Enum.GetValues(typeof(ContentType)).Cast<object>().ToArray());
            cboContentType.SelectedIndex = 0;

            // Initialize Compression ComboBox
            cboCompression.Items.AddRange(Enum.GetValues(typeof(CompressionType)).Cast<object>().ToArray());
            cboCompression.SelectedIndex = 0;

            // Initialize Encryption ComboBox
            cboEncryption.Items.AddRange(Enum.GetValues(typeof(EncryptionType)).Cast<object>().ToArray());
            cboEncryption.SelectedIndex = 0;

            // Initialize Estimate Method ComboBox
            cboEstimateMethod.Items.AddRange(Enum.GetValues(typeof(EstimateMethod)).Cast<object>().ToArray());
            cboEstimateMethod.SelectedIndex = 0;

            // Initialize Parallel Processes NumericUpDown
            numParallelProcesses.Minimum = 1;
            numParallelProcesses.Maximum = 32;
            numParallelProcesses.Value = 1;

            // Initialize Sample Percentage NumericUpDown
            numSamplePercentage.Minimum = 0;
            numSamplePercentage.Maximum = 100;
            numSamplePercentage.Value = 0;

            // Initialize Status Interval NumericUpDown
            numStatusInterval.Minimum = 0;
            numStatusInterval.Maximum = 3600;
            numStatusInterval.Value = 0;

            // Set default values
            txtDumpFileName.Text = "export_$(date +%Y%m%d_%H%M%S).dmp";
            txtLogFileName.Text = "export_$(date +%Y%m%d_%H%M%S).log";
            txtOracleSid.Text = "ORCL";
            txtOracleHome.Text = "/u01/app/oracle/product/19.0.0/dbhome_1";
            txtDatabaseName.Text = "localhost:1521/ORCL";
            txtDataPumpJobName.Text = "EXPORT_JOB_$(date +%Y%m%d_%H%M%S)";

            // Wire up event handlers
            chkFullExport.CheckedChanged += ChkFullExport_CheckedChanged;
            cboEncryption.SelectedIndexChanged += CboEncryption_SelectedIndexChanged;
        }

        private void ChkFullExport_CheckedChanged(object? sender, EventArgs e)
        {
            // Show/hide schema/table name field based on full export selection
            lblSchemaTableName.Visible = !chkFullExport.Checked;
            txtSchemaTableName.Visible = !chkFullExport.Checked;
            
            if (chkFullExport.Checked)
            {
                txtSchemaTableName.Text = string.Empty;
            }
        }

        private void CboEncryption_SelectedIndexChanged(object? sender, EventArgs e)
        {
            // Show/hide encryption password field based on encryption type
            bool showPassword = cboEncryption.SelectedItem is EncryptionType encryptionType && 
                               encryptionType == EncryptionType.Password;
            lblEncryptionPassword.Visible = showPassword;
            txtEncryptionPassword.Visible = showPassword;
        }

        private void BtnGenerateScript_Click(object? sender, EventArgs e)
        {
            if (ValidateForm())
            {
                SaveExportJob();
                GenerateKshScript();
            }
        }

        private void BtnSave_Click(object? sender, EventArgs e)
        {
            if (ValidateForm())
            {
                SaveExportJob();
                this.DialogResult = DialogResult.OK;
                this.Close();
            }
        }

        private void BtnCancel_Click(object? sender, EventArgs e)
        {
            this.DialogResult = DialogResult.Cancel;
            this.Close();
        }

        private bool ValidateForm()
        {
            if (string.IsNullOrWhiteSpace(txtJobName.Text))
            {
                MessageBox.Show("Job name is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtJobName.Focus();
                return false;
            }

            if (string.IsNullOrWhiteSpace(txtSqlQuery.Text))
            {
                MessageBox.Show("SQL query is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtSqlQuery.Focus();
                return false;
            }

            if (string.IsNullOrWhiteSpace(cboOracleDirectory.Text))
            {
                MessageBox.Show("Oracle directory object is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                cboOracleDirectory.Focus();
                return false;
            }

            if (string.IsNullOrWhiteSpace(txtDumpFileName.Text))
            {
                MessageBox.Show("Dump file name is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtDumpFileName.Focus();
                return false;
            }

            if (string.IsNullOrWhiteSpace(txtLogFileName.Text))
            {
                MessageBox.Show("Log file name is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtLogFileName.Focus();
                return false;
            }

            if (!chkFullExport.Checked && string.IsNullOrWhiteSpace(txtSchemaTableName.Text))
            {
                MessageBox.Show("Schema/Table name is required when not performing a full export.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtSchemaTableName.Focus();
                return false;
            }

            if (string.IsNullOrWhiteSpace(txtUsername.Text))
            {
                MessageBox.Show("Database username is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtUsername.Focus();
                return false;
            }

            if (string.IsNullOrWhiteSpace(txtPassword.Text))
            {
                MessageBox.Show("Database password is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtPassword.Focus();
                return false;
            }

            if (string.IsNullOrWhiteSpace(txtDatabaseName.Text))
            {
                MessageBox.Show("Database name is required.", "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                txtDatabaseName.Focus();
                return false;
            }

            return true;
        }

        private void LoadExportJob()
        {
            txtJobName.Text = ExportJob.JobName;
            txtDescription.Text = ExportJob.Description;
            txtSqlQuery.Text = ExportJob.SqlQuery;
            cboOracleDirectory.Text = ExportJob.OracleDirectory;
            txtDumpFileName.Text = ExportJob.DumpFileName;
            txtLogFileName.Text = ExportJob.LogFileName;
            numParallelProcesses.Value = ExportJob.ParallelProcesses;
            chkFullExport.Checked = ExportJob.IsFullExport;
            txtSchemaTableName.Text = ExportJob.SchemaTableName;
            cboExportType.SelectedItem = ExportJob.ExportType;
            cboContentType.SelectedItem = ExportJob.ContentType;
            cboCompression.SelectedItem = ExportJob.CompressionType;
            cboEncryption.SelectedItem = ExportJob.EncryptionType;
            txtEncryptionPassword.Text = ExportJob.EncryptionPassword;
            txtExcludeObjects.Text = ExportJob.ExcludeObjects;
            txtIncludeObjects.Text = ExportJob.IncludeObjects;
            chkEstimateOnly.Checked = ExportJob.EstimateOnly;
            cboEstimateMethod.SelectedItem = ExportJob.EstimateMethod;
            txtFlashbackTime.Text = ExportJob.FlashbackTime;
            txtFlashbackScn.Text = ExportJob.FlashbackScn;
            numSamplePercentage.Value = ExportJob.SamplePercentage;
            numStatusInterval.Value = ExportJob.StatusInterval;
            txtDataPumpJobName.Text = ExportJob.DataPumpJobName;
            txtOracleSid.Text = ExportJob.OracleSid;
            txtOracleHome.Text = ExportJob.OracleHome;
            txtUsername.Text = ExportJob.Username;
            txtPassword.Text = ExportJob.Password;
            txtDatabaseName.Text = ExportJob.DatabaseName;
        }

        private void SaveExportJob()
        {
            ExportJob.JobName = txtJobName.Text.Trim();
            ExportJob.Description = txtDescription.Text.Trim();
            ExportJob.SqlQuery = txtSqlQuery.Text.Trim();
            ExportJob.OracleDirectory = cboOracleDirectory.Text.Trim();
            ExportJob.DumpFileName = txtDumpFileName.Text.Trim();
            ExportJob.LogFileName = txtLogFileName.Text.Trim();
            ExportJob.ParallelProcesses = (int)numParallelProcesses.Value;
            ExportJob.IsFullExport = chkFullExport.Checked;
            ExportJob.SchemaTableName = txtSchemaTableName.Text.Trim();
            ExportJob.ExportType = cboExportType.SelectedItem is ExportType exportType ? exportType : ExportType.Schemas;
            ExportJob.ContentType = cboContentType.SelectedItem is ContentType contentType ? contentType : ContentType.All;
            ExportJob.CompressionType = cboCompression.SelectedItem is CompressionType compressionType ? compressionType : CompressionType.None;
            ExportJob.EncryptionType = cboEncryption.SelectedItem is EncryptionType encryptionType ? encryptionType : EncryptionType.None;
            ExportJob.EncryptionPassword = txtEncryptionPassword.Text.Trim();
            ExportJob.ExcludeObjects = txtExcludeObjects.Text.Trim();
            ExportJob.IncludeObjects = txtIncludeObjects.Text.Trim();
            ExportJob.EstimateOnly = chkEstimateOnly.Checked;
            ExportJob.EstimateMethod = cboEstimateMethod.SelectedItem is EstimateMethod estimateMethod ? estimateMethod : EstimateMethod.Blocks;
            ExportJob.FlashbackTime = txtFlashbackTime.Text.Trim();
            ExportJob.FlashbackScn = txtFlashbackScn.Text.Trim();
            ExportJob.SamplePercentage = (int)numSamplePercentage.Value;
            ExportJob.StatusInterval = (int)numStatusInterval.Value;
            ExportJob.DataPumpJobName = txtDataPumpJobName.Text.Trim();
            ExportJob.OracleSid = txtOracleSid.Text.Trim();
            ExportJob.OracleHome = txtOracleHome.Text.Trim();
            ExportJob.Username = txtUsername.Text.Trim();
            ExportJob.Password = txtPassword.Text.Trim();
            ExportJob.DatabaseName = txtDatabaseName.Text.Trim();
            ExportJob.LastModified = DateTime.Now;
        }

        private void GenerateKshScript()
        {
            var script = GenerateKshScriptContent();
            
            using (var saveDialog = new SaveFileDialog())
            {
                saveDialog.Filter = "KornShell files (*.ksh)|*.ksh|Bash files (*.sh)|*.sh|All files (*.*)|*.*";
                saveDialog.Title = "Save KornShell Script";
                saveDialog.FileName = $"{ExportJob.JobName.Replace(" ", "_")}_export.ksh";
                
                if (saveDialog.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        File.WriteAllText(saveDialog.FileName, script);
                        MessageBox.Show($"KornShell script generated successfully!\nSaved to: {saveDialog.FileName}", 
                            "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show($"Error saving script: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                }
            }
        }

        private string GenerateKshScriptContent()
        {
            var script = $@"#!/bin/ksh

# Oracle Data Pump Export Script
# Generated on: {DateTime.Now:yyyy-MM-dd HH:mm:ss}
# Job Name: {ExportJob.JobName}
# Description: {ExportJob.Description}

# Oracle environment variables
export ORACLE_SID={ExportJob.OracleSid}
export ORACLE_HOME={ExportJob.OracleHome}
export PATH=$ORACLE_HOME/bin:$PATH

# Define Data Pump export parameters
DIRECTORY={ExportJob.OracleDirectory}
DUMPFILE=${{1:-{ExportJob.DumpFileName}}}
LOGFILE=${{2:-{ExportJob.LogFileName}}}
QUERY=""${{3:-{ExportJob.SqlQuery.Replace("\"", "\\\"")}}}""

# Export parameters
PARALLEL=${{4:-{ExportJob.ParallelProcesses}}}  # Default {ExportJob.ParallelProcesses} parallel process(es)
JOB_NAME=""{ExportJob.DataPumpJobName}""

# Database connection parameters
USERNAME={ExportJob.Username}
PASSWORD={ExportJob.Password}
DATABASE={ExportJob.DatabaseName}

echo ""Starting Oracle Data Pump Export...""
echo ""Job: {ExportJob.JobName}""
echo ""Directory: $DIRECTORY""
echo ""Dump File: $DUMPFILE""
echo ""Log File: $LOGFILE""
echo ""Parallel Processes: $PARALLEL""
echo ""Database: $DATABASE""
echo ""Query: $QUERY""
echo ""----------------------------------------""

# Build the expdp command
EXPDP_CMD=""expdp $USERNAME/$PASSWORD@$DATABASE DIRECTORY=$DIRECTORY DUMPFILE=$DUMPFILE LOGFILE=$LOGFILE PARALLEL=$PARALLEL""

# Add job name if specified
if [ -n ""$JOB_NAME"" ]; then
    EXPDP_CMD=""$EXPDP_CMD JOB_NAME=$JOB_NAME""
fi

# Add export type based on configuration
case ""{ExportJob.ExportType}"" in
    ""Schemas"")
        if [ -n ""{ExportJob.SchemaTableName}"" ]; then
            EXPDP_CMD=""$EXPDP_CMD SCHEMAS={ExportJob.SchemaTableName}""
        fi
        ;;
    ""Tables"")
        if [ -n ""{ExportJob.SchemaTableName}"" ]; then
            EXPDP_CMD=""$EXPDP_CMD TABLES={ExportJob.SchemaTableName}""
        fi
        ;;
    ""Full"")
        EXPDP_CMD=""$EXPDP_CMD FULL=Y""
        ;;
    ""TransportableTablespaces"")
        if [ -n ""{ExportJob.SchemaTableName}"" ]; then
            EXPDP_CMD=""$EXPDP_CMD TRANSPORT_TABLESPACES={ExportJob.SchemaTableName}""
        fi
        ;;
    ""Tablespaces"")
        if [ -n ""{ExportJob.SchemaTableName}"" ]; then
            EXPDP_CMD=""$EXPDP_CMD TABLESPACES={ExportJob.SchemaTableName}""
        fi
        ;;
esac

# Add content type
case ""{ExportJob.ContentType}"" in
    ""DataOnly"")
        EXPDP_CMD=""$EXPDP_CMD CONTENT=DATA_ONLY""
        ;;
    ""MetadataOnly"")
        EXPDP_CMD=""$EXPDP_CMD CONTENT=METADATA_ONLY""
        ;;
    ""All"")
        # Default is ALL, no need to specify
        ;;
esac

# Add compression
case ""{ExportJob.CompressionType}"" in
    ""Basic"")
        EXPDP_CMD=""$EXPDP_CMD COMPRESSION=BASIC""
        ;;
    ""Low"")
        EXPDP_CMD=""$EXPDP_CMD COMPRESSION=LOW""
        ;;
    ""Medium"")
        EXPDP_CMD=""$EXPDP_CMD COMPRESSION=MEDIUM""
        ;;
    ""High"")
        EXPDP_CMD=""$EXPDP_CMD COMPRESSION=HIGH""
        ;;
    ""None"")
        # No compression
        ;;
esac

# Add encryption
case ""{ExportJob.EncryptionType}"" in
    ""Password"")
        if [ -n ""{ExportJob.EncryptionPassword}"" ]; then
            EXPDP_CMD=""$EXPDP_CMD ENCRYPTION_PASSWORD={ExportJob.EncryptionPassword}""
        fi
        ;;
    ""Transparent"")
        EXPDP_CMD=""$EXPDP_CMD ENCRYPTION=TRANSPARENT""
        ;;
    ""Dual"")
        EXPDP_CMD=""$EXPDP_CMD ENCRYPTION=DUAL""
        ;;
    ""None"")
        # No encryption
        ;;
esac

# Add exclude objects if specified
if [ -n ""{ExportJob.ExcludeObjects}"" ]; then
    EXPDP_CMD=""$EXPDP_CMD EXCLUDE={ExportJob.ExcludeObjects}""
fi

# Add include objects if specified
if [ -n ""{ExportJob.IncludeObjects}"" ]; then
    EXPDP_CMD=""$EXPDP_CMD INCLUDE={ExportJob.IncludeObjects}""
fi

# Add estimate only if specified
if [ ""{ExportJob.EstimateOnly.ToString().ToLower()}"" = ""true"" ]; then
    EXPDP_CMD=""$EXPDP_CMD ESTIMATE_ONLY=YES""
    case ""{ExportJob.EstimateMethod}"" in
        ""Blocks"")
            EXPDP_CMD=""$EXPDP_CMD ESTIMATE=BLOCKS""
            ;;
        ""Statistics"")
            EXPDP_CMD=""$EXPDP_CMD ESTIMATE=STATISTICS""
            ;;
    esac
fi

# Add flashback time if specified
if [ -n ""{ExportJob.FlashbackTime}"" ]; then
    EXPDP_CMD=""$EXPDP_CMD FLASHBACK_TIME=\""{ExportJob.FlashbackTime}\""""
fi

# Add flashback SCN if specified
if [ -n ""{ExportJob.FlashbackScn}"" ]; then
    EXPDP_CMD=""$EXPDP_CMD FLASHBACK_SCN={ExportJob.FlashbackScn}""
fi

# Add sample percentage if specified
if [ {ExportJob.SamplePercentage} -gt 0 ]; then
    EXPDP_CMD=""$EXPDP_CMD SAMPLE={ExportJob.SamplePercentage}""
fi

# Add status interval if specified
if [ {ExportJob.StatusInterval} -gt 0 ]; then
    EXPDP_CMD=""$EXPDP_CMD STATUS={ExportJob.StatusInterval}""
fi

# Add query if provided
if [ -n ""$QUERY"" ]; then
    EXPDP_CMD=""$EXPDP_CMD QUERY=\""$QUERY\""""
fi

echo ""Executing: $EXPDP_CMD""
echo ""----------------------------------------""

# Run Data Pump export
$EXPDP_CMD

# Check for success
if [ $? -eq 0 ]; then
    echo ""----------------------------------------""
    echo ""Export completed successfully.""
    echo ""Dump file: $DUMPFILE""
    echo ""Log file: $LOGFILE""
    echo ""Completed at: $(date)""
else
    echo ""----------------------------------------""
    echo ""Export encountered an error. Check log file for details.""
    echo ""Log file: $LOGFILE""
    exit 1
fi
";

            return script;
        }
    }
} 