namespace ControlFileGenerator.WinForms.Forms
{
    partial class DataPumpExportForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.lblJobName = new System.Windows.Forms.Label();
            this.txtJobName = new System.Windows.Forms.TextBox();
            this.lblDescription = new System.Windows.Forms.Label();
            this.txtDescription = new System.Windows.Forms.TextBox();
            this.lblSqlQuery = new System.Windows.Forms.Label();
            this.txtSqlQuery = new System.Windows.Forms.TextBox();
            this.lblOracleDirectory = new System.Windows.Forms.Label();
            this.cboOracleDirectory = new System.Windows.Forms.ComboBox();
            this.lblDumpFileName = new System.Windows.Forms.Label();
            this.txtDumpFileName = new System.Windows.Forms.TextBox();
            this.lblLogFileName = new System.Windows.Forms.Label();
            this.txtLogFileName = new System.Windows.Forms.TextBox();
            this.lblParallelProcesses = new System.Windows.Forms.Label();
            this.numParallelProcesses = new System.Windows.Forms.NumericUpDown();
            this.chkFullExport = new System.Windows.Forms.CheckBox();
            this.lblSchemaTableName = new System.Windows.Forms.Label();
            this.txtSchemaTableName = new System.Windows.Forms.TextBox();
            this.lblExportType = new System.Windows.Forms.Label();
            this.cboExportType = new System.Windows.Forms.ComboBox();
            this.lblContentType = new System.Windows.Forms.Label();
            this.cboContentType = new System.Windows.Forms.ComboBox();
            this.lblCompression = new System.Windows.Forms.Label();
            this.cboCompression = new System.Windows.Forms.ComboBox();
            this.lblEncryption = new System.Windows.Forms.Label();
            this.cboEncryption = new System.Windows.Forms.ComboBox();
            this.lblEncryptionPassword = new System.Windows.Forms.Label();
            this.txtEncryptionPassword = new System.Windows.Forms.TextBox();
            this.lblExcludeObjects = new System.Windows.Forms.Label();
            this.txtExcludeObjects = new System.Windows.Forms.TextBox();
            this.lblIncludeObjects = new System.Windows.Forms.Label();
            this.txtIncludeObjects = new System.Windows.Forms.TextBox();
            this.chkEstimateOnly = new System.Windows.Forms.CheckBox();
            this.lblEstimateMethod = new System.Windows.Forms.Label();
            this.cboEstimateMethod = new System.Windows.Forms.ComboBox();
            this.lblFlashbackTime = new System.Windows.Forms.Label();
            this.txtFlashbackTime = new System.Windows.Forms.TextBox();
            this.lblFlashbackScn = new System.Windows.Forms.Label();
            this.txtFlashbackScn = new System.Windows.Forms.TextBox();
            this.lblSamplePercentage = new System.Windows.Forms.Label();
            this.numSamplePercentage = new System.Windows.Forms.NumericUpDown();
            this.lblStatusInterval = new System.Windows.Forms.Label();
            this.numStatusInterval = new System.Windows.Forms.NumericUpDown();
            this.lblDataPumpJobName = new System.Windows.Forms.Label();
            this.txtDataPumpJobName = new System.Windows.Forms.TextBox();
            this.lblOracleSid = new System.Windows.Forms.Label();
            this.txtOracleSid = new System.Windows.Forms.TextBox();
            this.lblOracleHome = new System.Windows.Forms.Label();
            this.txtOracleHome = new System.Windows.Forms.TextBox();
            this.lblUsername = new System.Windows.Forms.Label();
            this.txtUsername = new System.Windows.Forms.TextBox();
            this.lblPassword = new System.Windows.Forms.Label();
            this.txtPassword = new System.Windows.Forms.TextBox();
            this.lblDatabaseName = new System.Windows.Forms.Label();
            this.txtDatabaseName = new System.Windows.Forms.TextBox();
            this.btnGenerateScript = new System.Windows.Forms.Button();
            this.btnSave = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.grpExportSettings = new System.Windows.Forms.GroupBox();
            this.grpDatabaseSettings = new System.Windows.Forms.GroupBox();
            this.grpJobSettings = new System.Windows.Forms.GroupBox();
            ((System.ComponentModel.ISupportInitialize)(this.numParallelProcesses)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numSamplePercentage)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numStatusInterval)).BeginInit();
            this.grpExportSettings.SuspendLayout();
            this.grpDatabaseSettings.SuspendLayout();
            this.grpJobSettings.SuspendLayout();
            this.SuspendLayout();
            // 
            // lblJobName
            // 
            this.lblJobName.AutoSize = true;
            this.lblJobName.Location = new System.Drawing.Point(12, 25);
            this.lblJobName.Name = "lblJobName";
            this.lblJobName.Size = new System.Drawing.Size(67, 15);
            this.lblJobName.TabIndex = 0;
            this.lblJobName.Text = "Job Name:";
            // 
            // txtJobName
            // 
            this.txtJobName.Location = new System.Drawing.Point(120, 22);
            this.txtJobName.Name = "txtJobName";
            this.txtJobName.Size = new System.Drawing.Size(300, 23);
            this.txtJobName.TabIndex = 1;
            // 
            // lblDescription
            // 
            this.lblDescription.AutoSize = true;
            this.lblDescription.Location = new System.Drawing.Point(12, 54);
            this.lblDescription.Name = "lblDescription";
            this.lblDescription.Size = new System.Drawing.Size(70, 15);
            this.lblDescription.TabIndex = 2;
            this.lblDescription.Text = "Description:";
            // 
            // txtDescription
            // 
            this.txtDescription.Location = new System.Drawing.Point(120, 51);
            this.txtDescription.Multiline = true;
            this.txtDescription.Name = "txtDescription";
            this.txtDescription.Size = new System.Drawing.Size(300, 60);
            this.txtDescription.TabIndex = 3;
            // 
            // lblSqlQuery
            // 
            this.lblSqlQuery.AutoSize = true;
            this.lblSqlQuery.Location = new System.Drawing.Point(12, 25);
            this.lblSqlQuery.Name = "lblSqlQuery";
            this.lblSqlQuery.Size = new System.Drawing.Size(67, 15);
            this.lblSqlQuery.TabIndex = 0;
            this.lblSqlQuery.Text = "SQL Query:";
            // 
            // txtSqlQuery
            // 
            this.txtSqlQuery.Location = new System.Drawing.Point(120, 22);
            this.txtSqlQuery.Multiline = true;
            this.txtSqlQuery.Name = "txtSqlQuery";
            this.txtSqlQuery.Size = new System.Drawing.Size(300, 60);
            this.txtSqlQuery.TabIndex = 1;
            // 
            // lblOracleDirectory
            // 
            this.lblOracleDirectory.AutoSize = true;
            this.lblOracleDirectory.Location = new System.Drawing.Point(12, 91);
            this.lblOracleDirectory.Name = "lblOracleDirectory";
            this.lblOracleDirectory.Size = new System.Drawing.Size(102, 15);
            this.lblOracleDirectory.TabIndex = 2;
            this.lblOracleDirectory.Text = "Oracle Directory:";
            // 
            // cboOracleDirectory
            // 
            this.cboOracleDirectory.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboOracleDirectory.FormattingEnabled = true;
            this.cboOracleDirectory.Location = new System.Drawing.Point(120, 88);
            this.cboOracleDirectory.Name = "cboOracleDirectory";
            this.cboOracleDirectory.Size = new System.Drawing.Size(200, 23);
            this.cboOracleDirectory.TabIndex = 3;
            // 
            // lblDumpFileName
            // 
            this.lblDumpFileName.AutoSize = true;
            this.lblDumpFileName.Location = new System.Drawing.Point(12, 120);
            this.lblDumpFileName.Name = "lblDumpFileName";
            this.lblDumpFileName.Size = new System.Drawing.Size(95, 15);
            this.lblDumpFileName.TabIndex = 4;
            this.lblDumpFileName.Text = "Dump File Name:";
            // 
            // txtDumpFileName
            // 
            this.txtDumpFileName.Location = new System.Drawing.Point(120, 117);
            this.txtDumpFileName.Name = "txtDumpFileName";
            this.txtDumpFileName.Size = new System.Drawing.Size(300, 23);
            this.txtDumpFileName.TabIndex = 5;
            // 
            // lblLogFileName
            // 
            this.lblLogFileName.AutoSize = true;
            this.lblLogFileName.Location = new System.Drawing.Point(12, 149);
            this.lblLogFileName.Name = "lblLogFileName";
            this.lblLogFileName.Size = new System.Drawing.Size(85, 15);
            this.lblLogFileName.TabIndex = 6;
            this.lblLogFileName.Text = "Log File Name:";
            // 
            // txtLogFileName
            // 
            this.txtLogFileName.Location = new System.Drawing.Point(120, 146);
            this.txtLogFileName.Name = "txtLogFileName";
            this.txtLogFileName.Size = new System.Drawing.Size(300, 23);
            this.txtLogFileName.TabIndex = 7;
            // 
            // lblParallelProcesses
            // 
            this.lblParallelProcesses.AutoSize = true;
            this.lblParallelProcesses.Location = new System.Drawing.Point(12, 178);
            this.lblParallelProcesses.Name = "lblParallelProcesses";
            this.lblParallelProcesses.Size = new System.Drawing.Size(108, 15);
            this.lblParallelProcesses.TabIndex = 8;
            this.lblParallelProcesses.Text = "Parallel Processes:";
            // 
            // numParallelProcesses
            // 
            this.numParallelProcesses.Location = new System.Drawing.Point(120, 175);
            this.numParallelProcesses.Maximum = new decimal(new int[] {
            32,
            0,
            0,
            0});
            this.numParallelProcesses.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numParallelProcesses.Name = "numParallelProcesses";
            this.numParallelProcesses.Size = new System.Drawing.Size(80, 23);
            this.numParallelProcesses.TabIndex = 9;
            this.numParallelProcesses.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // chkFullExport
            // 
            this.chkFullExport.AutoSize = true;
            this.chkFullExport.Location = new System.Drawing.Point(12, 204);
            this.chkFullExport.Name = "chkFullExport";
            this.chkFullExport.Size = new System.Drawing.Size(95, 19);
            this.chkFullExport.TabIndex = 10;
            this.chkFullExport.Text = "Full Export?";
            this.chkFullExport.UseVisualStyleBackColor = true;
            // 
            // lblSchemaTableName
            // 
            this.lblSchemaTableName.AutoSize = true;
            this.lblSchemaTableName.Location = new System.Drawing.Point(12, 233);
            this.lblSchemaTableName.Name = "lblSchemaTableName";
            this.lblSchemaTableName.Size = new System.Drawing.Size(120, 15);
            this.lblSchemaTableName.TabIndex = 11;
            this.lblSchemaTableName.Text = "Schema/Table Name:";
            // 
            // txtSchemaTableName
            // 
            this.txtSchemaTableName.Location = new System.Drawing.Point(120, 230);
            this.txtSchemaTableName.Name = "txtSchemaTableName";
            this.txtSchemaTableName.Size = new System.Drawing.Size(200, 23);
            this.txtSchemaTableName.TabIndex = 12;
            // 
            // lblExportType
            // 
            this.lblExportType.AutoSize = true;
            this.lblExportType.Location = new System.Drawing.Point(12, 262);
            this.lblExportType.Name = "lblExportType";
            this.lblExportType.Size = new System.Drawing.Size(75, 15);
            this.lblExportType.TabIndex = 13;
            this.lblExportType.Text = "Export Type:";
            // 
            // cboExportType
            // 
            this.cboExportType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboExportType.FormattingEnabled = true;
            this.cboExportType.Location = new System.Drawing.Point(120, 259);
            this.cboExportType.Name = "cboExportType";
            this.cboExportType.Size = new System.Drawing.Size(200, 23);
            this.cboExportType.TabIndex = 14;
            // 
            // lblContentType
            // 
            this.lblContentType.AutoSize = true;
            this.lblContentType.Location = new System.Drawing.Point(12, 291);
            this.lblContentType.Name = "lblContentType";
            this.lblContentType.Size = new System.Drawing.Size(85, 15);
            this.lblContentType.TabIndex = 15;
            this.lblContentType.Text = "Content Type:";
            // 
            // cboContentType
            // 
            this.cboContentType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboContentType.FormattingEnabled = true;
            this.cboContentType.Location = new System.Drawing.Point(120, 288);
            this.cboContentType.Name = "cboContentType";
            this.cboContentType.Size = new System.Drawing.Size(200, 23);
            this.cboContentType.TabIndex = 16;
            // 
            // lblCompression
            // 
            this.lblCompression.AutoSize = true;
            this.lblCompression.Location = new System.Drawing.Point(12, 320);
            this.lblCompression.Name = "lblCompression";
            this.lblCompression.Size = new System.Drawing.Size(80, 15);
            this.lblCompression.TabIndex = 17;
            this.lblCompression.Text = "Compression:";
            // 
            // cboCompression
            // 
            this.cboCompression.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboCompression.FormattingEnabled = true;
            this.cboCompression.Location = new System.Drawing.Point(120, 317);
            this.cboCompression.Name = "cboCompression";
            this.cboCompression.Size = new System.Drawing.Size(200, 23);
            this.cboCompression.TabIndex = 18;
            // 
            // lblEncryption
            // 
            this.lblEncryption.AutoSize = true;
            this.lblEncryption.Location = new System.Drawing.Point(12, 349);
            this.lblEncryption.Name = "lblEncryption";
            this.lblEncryption.Size = new System.Drawing.Size(70, 15);
            this.lblEncryption.TabIndex = 19;
            this.lblEncryption.Text = "Encryption:";
            // 
            // cboEncryption
            // 
            this.cboEncryption.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboEncryption.FormattingEnabled = true;
            this.cboEncryption.Location = new System.Drawing.Point(120, 346);
            this.cboEncryption.Name = "cboEncryption";
            this.cboEncryption.Size = new System.Drawing.Size(200, 23);
            this.cboEncryption.TabIndex = 20;
            // 
            // lblEncryptionPassword
            // 
            this.lblEncryptionPassword.AutoSize = true;
            this.lblEncryptionPassword.Location = new System.Drawing.Point(12, 378);
            this.lblEncryptionPassword.Name = "lblEncryptionPassword";
            this.lblEncryptionPassword.Size = new System.Drawing.Size(120, 15);
            this.lblEncryptionPassword.TabIndex = 21;
            this.lblEncryptionPassword.Text = "Encryption Password:";
            // 
            // txtEncryptionPassword
            // 
            this.txtEncryptionPassword.Location = new System.Drawing.Point(140, 375);
            this.txtEncryptionPassword.Name = "txtEncryptionPassword";
            this.txtEncryptionPassword.PasswordChar = '*';
            this.txtEncryptionPassword.Size = new System.Drawing.Size(180, 23);
            this.txtEncryptionPassword.TabIndex = 22;
            // 
            // lblExcludeObjects
            // 
            this.lblExcludeObjects.AutoSize = true;
            this.lblExcludeObjects.Location = new System.Drawing.Point(350, 25);
            this.lblExcludeObjects.Name = "lblExcludeObjects";
            this.lblExcludeObjects.Size = new System.Drawing.Size(95, 15);
            this.lblExcludeObjects.TabIndex = 23;
            this.lblExcludeObjects.Text = "Exclude Objects:";
            // 
            // txtExcludeObjects
            // 
            this.txtExcludeObjects.Location = new System.Drawing.Point(450, 22);
            this.txtExcludeObjects.Multiline = true;
            this.txtExcludeObjects.Name = "txtExcludeObjects";
            this.txtExcludeObjects.Size = new System.Drawing.Size(300, 60);
            this.txtExcludeObjects.TabIndex = 24;
            // 
            // lblIncludeObjects
            // 
            this.lblIncludeObjects.AutoSize = true;
            this.lblIncludeObjects.Location = new System.Drawing.Point(350, 91);
            this.lblIncludeObjects.Name = "lblIncludeObjects";
            this.lblIncludeObjects.Size = new System.Drawing.Size(95, 15);
            this.lblIncludeObjects.TabIndex = 25;
            this.lblIncludeObjects.Text = "Include Objects:";
            // 
            // txtIncludeObjects
            // 
            this.txtIncludeObjects.Location = new System.Drawing.Point(450, 88);
            this.txtIncludeObjects.Multiline = true;
            this.txtIncludeObjects.Name = "txtIncludeObjects";
            this.txtIncludeObjects.Size = new System.Drawing.Size(300, 60);
            this.txtIncludeObjects.TabIndex = 26;
            // 
            // chkEstimateOnly
            // 
            this.chkEstimateOnly.AutoSize = true;
            this.chkEstimateOnly.Location = new System.Drawing.Point(350, 158);
            this.chkEstimateOnly.Name = "chkEstimateOnly";
            this.chkEstimateOnly.Size = new System.Drawing.Size(100, 19);
            this.chkEstimateOnly.TabIndex = 27;
            this.chkEstimateOnly.Text = "Estimate Only";
            this.chkEstimateOnly.UseVisualStyleBackColor = true;
            // 
            // lblEstimateMethod
            // 
            this.lblEstimateMethod.AutoSize = true;
            this.lblEstimateMethod.Location = new System.Drawing.Point(350, 187);
            this.lblEstimateMethod.Name = "lblEstimateMethod";
            this.lblEstimateMethod.Size = new System.Drawing.Size(100, 15);
            this.lblEstimateMethod.TabIndex = 28;
            this.lblEstimateMethod.Text = "Estimate Method:";
            // 
            // cboEstimateMethod
            // 
            this.cboEstimateMethod.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboEstimateMethod.FormattingEnabled = true;
            this.cboEstimateMethod.Location = new System.Drawing.Point(450, 184);
            this.cboEstimateMethod.Name = "cboEstimateMethod";
            this.cboEstimateMethod.Size = new System.Drawing.Size(150, 23);
            this.cboEstimateMethod.TabIndex = 29;
            // 
            // lblFlashbackTime
            // 
            this.lblFlashbackTime.AutoSize = true;
            this.lblFlashbackTime.Location = new System.Drawing.Point(350, 216);
            this.lblFlashbackTime.Name = "lblFlashbackTime";
            this.lblFlashbackTime.Size = new System.Drawing.Size(95, 15);
            this.lblFlashbackTime.TabIndex = 30;
            this.lblFlashbackTime.Text = "Flashback Time:";
            // 
            // txtFlashbackTime
            // 
            this.txtFlashbackTime.Location = new System.Drawing.Point(450, 213);
            this.txtFlashbackTime.Name = "txtFlashbackTime";
            this.txtFlashbackTime.Size = new System.Drawing.Size(200, 23);
            this.txtFlashbackTime.TabIndex = 31;
            // 
            // lblFlashbackScn
            // 
            this.lblFlashbackScn.AutoSize = true;
            this.lblFlashbackScn.Location = new System.Drawing.Point(350, 245);
            this.lblFlashbackScn.Name = "lblFlashbackScn";
            this.lblFlashbackScn.Size = new System.Drawing.Size(85, 15);
            this.lblFlashbackScn.TabIndex = 32;
            this.lblFlashbackScn.Text = "Flashback SCN:";
            // 
            // txtFlashbackScn
            // 
            this.txtFlashbackScn.Location = new System.Drawing.Point(450, 242);
            this.txtFlashbackScn.Name = "txtFlashbackScn";
            this.txtFlashbackScn.Size = new System.Drawing.Size(200, 23);
            this.txtFlashbackScn.TabIndex = 33;
            // 
            // lblSamplePercentage
            // 
            this.lblSamplePercentage.AutoSize = true;
            this.lblSamplePercentage.Location = new System.Drawing.Point(350, 274);
            this.lblSamplePercentage.Name = "lblSamplePercentage";
            this.lblSamplePercentage.Size = new System.Drawing.Size(110, 15);
            this.lblSamplePercentage.TabIndex = 34;
            this.lblSamplePercentage.Text = "Sample Percentage:";
            // 
            // numSamplePercentage
            // 
            this.numSamplePercentage.Location = new System.Drawing.Point(450, 271);
            this.numSamplePercentage.Name = "numSamplePercentage";
            this.numSamplePercentage.Size = new System.Drawing.Size(80, 23);
            this.numSamplePercentage.TabIndex = 35;
            // 
            // lblStatusInterval
            // 
            this.lblStatusInterval.AutoSize = true;
            this.lblStatusInterval.Location = new System.Drawing.Point(350, 303);
            this.lblStatusInterval.Name = "lblStatusInterval";
            this.lblStatusInterval.Size = new System.Drawing.Size(90, 15);
            this.lblStatusInterval.TabIndex = 36;
            this.lblStatusInterval.Text = "Status Interval:";
            // 
            // numStatusInterval
            // 
            this.numStatusInterval.Location = new System.Drawing.Point(450, 300);
            this.numStatusInterval.Name = "numStatusInterval";
            this.numStatusInterval.Size = new System.Drawing.Size(80, 23);
            this.numStatusInterval.TabIndex = 37;
            // 
            // lblDataPumpJobName
            // 
            this.lblDataPumpJobName.AutoSize = true;
            this.lblDataPumpJobName.Location = new System.Drawing.Point(350, 332);
            this.lblDataPumpJobName.Name = "lblDataPumpJobName";
            this.lblDataPumpJobName.Size = new System.Drawing.Size(65, 15);
            this.lblDataPumpJobName.TabIndex = 38;
            this.lblDataPumpJobName.Text = "Job Name:";
            // 
            // txtDataPumpJobName
            // 
            this.txtDataPumpJobName.Location = new System.Drawing.Point(450, 329);
            this.txtDataPumpJobName.Name = "txtDataPumpJobName";
            this.txtDataPumpJobName.Size = new System.Drawing.Size(200, 23);
            this.txtDataPumpJobName.TabIndex = 39;
            // 
            // lblOracleSid
            // 
            this.lblOracleSid.AutoSize = true;
            this.lblOracleSid.Location = new System.Drawing.Point(12, 25);
            this.lblOracleSid.Name = "lblOracleSid";
            this.lblOracleSid.Size = new System.Drawing.Size(70, 15);
            this.lblOracleSid.TabIndex = 0;
            this.lblOracleSid.Text = "Oracle SID:";
            // 
            // txtOracleSid
            // 
            this.txtOracleSid.Location = new System.Drawing.Point(120, 22);
            this.txtOracleSid.Name = "txtOracleSid";
            this.txtOracleSid.Size = new System.Drawing.Size(150, 23);
            this.txtOracleSid.TabIndex = 1;
            // 
            // lblOracleHome
            // 
            this.lblOracleHome.AutoSize = true;
            this.lblOracleHome.Location = new System.Drawing.Point(12, 54);
            this.lblOracleHome.Name = "lblOracleHome";
            this.lblOracleHome.Size = new System.Drawing.Size(85, 15);
            this.lblOracleHome.TabIndex = 2;
            this.lblOracleHome.Text = "Oracle Home:";
            // 
            // txtOracleHome
            // 
            this.txtOracleHome.Location = new System.Drawing.Point(120, 51);
            this.txtOracleHome.Name = "txtOracleHome";
            this.txtOracleHome.Size = new System.Drawing.Size(300, 23);
            this.txtOracleHome.TabIndex = 3;
            // 
            // lblUsername
            // 
            this.lblUsername.AutoSize = true;
            this.lblUsername.Location = new System.Drawing.Point(12, 83);
            this.lblUsername.Name = "lblUsername";
            this.lblUsername.Size = new System.Drawing.Size(68, 15);
            this.lblUsername.TabIndex = 4;
            this.lblUsername.Text = "Username:";
            // 
            // txtUsername
            // 
            this.txtUsername.Location = new System.Drawing.Point(120, 80);
            this.txtUsername.Name = "txtUsername";
            this.txtUsername.Size = new System.Drawing.Size(150, 23);
            this.txtUsername.TabIndex = 5;
            // 
            // lblPassword
            // 
            this.lblPassword.AutoSize = true;
            this.lblPassword.Location = new System.Drawing.Point(12, 112);
            this.lblPassword.Name = "lblPassword";
            this.lblPassword.Size = new System.Drawing.Size(63, 15);
            this.lblPassword.TabIndex = 6;
            this.lblPassword.Text = "Password:";
            // 
            // txtPassword
            // 
            this.txtPassword.Location = new System.Drawing.Point(120, 109);
            this.txtPassword.Name = "txtPassword";
            this.txtPassword.PasswordChar = '*';
            this.txtPassword.Size = new System.Drawing.Size(150, 23);
            this.txtPassword.TabIndex = 7;
            // 
            // lblDatabaseName
            // 
            this.lblDatabaseName.AutoSize = true;
            this.lblDatabaseName.Location = new System.Drawing.Point(12, 141);
            this.lblDatabaseName.Name = "lblDatabaseName";
            this.lblDatabaseName.Size = new System.Drawing.Size(98, 15);
            this.lblDatabaseName.TabIndex = 8;
            this.lblDatabaseName.Text = "Database Name:";
            // 
            // txtDatabaseName
            // 
            this.txtDatabaseName.Location = new System.Drawing.Point(120, 138);
            this.txtDatabaseName.Name = "txtDatabaseName";
            this.txtDatabaseName.Size = new System.Drawing.Size(200, 23);
            this.txtDatabaseName.TabIndex = 9;
            // 
            // btnGenerateScript
            // 
            this.btnGenerateScript.Location = new System.Drawing.Point(12, 720);
            this.btnGenerateScript.Name = "btnGenerateScript";
            this.btnGenerateScript.Size = new System.Drawing.Size(140, 35);
            this.btnGenerateScript.TabIndex = 0;
            this.btnGenerateScript.Text = "Generate Script";
            this.btnGenerateScript.UseVisualStyleBackColor = true;
            this.btnGenerateScript.Click += new System.EventHandler(this.BtnGenerateScript_Click);
            // 
            // btnSave
            // 
            this.btnSave.Location = new System.Drawing.Point(170, 720);
            this.btnSave.Name = "btnSave";
            this.btnSave.Size = new System.Drawing.Size(100, 35);
            this.btnSave.TabIndex = 1;
            this.btnSave.Text = "Save";
            this.btnSave.UseVisualStyleBackColor = true;
            this.btnSave.Click += new System.EventHandler(this.BtnSave_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.Location = new System.Drawing.Point(290, 720);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(100, 35);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "Cancel";
            this.btnCancel.UseVisualStyleBackColor = true;
            this.btnCancel.Click += new System.EventHandler(this.BtnCancel_Click);
            // 
            // grpExportSettings
            // 
            this.grpExportSettings.Controls.Add(this.lblSqlQuery);
            this.grpExportSettings.Controls.Add(this.txtSqlQuery);
            this.grpExportSettings.Controls.Add(this.lblOracleDirectory);
            this.grpExportSettings.Controls.Add(this.cboOracleDirectory);
            this.grpExportSettings.Controls.Add(this.lblDumpFileName);
            this.grpExportSettings.Controls.Add(this.txtDumpFileName);
            this.grpExportSettings.Controls.Add(this.lblLogFileName);
            this.grpExportSettings.Controls.Add(this.txtLogFileName);
            this.grpExportSettings.Controls.Add(this.lblParallelProcesses);
            this.grpExportSettings.Controls.Add(this.numParallelProcesses);
            this.grpExportSettings.Controls.Add(this.chkFullExport);
            this.grpExportSettings.Controls.Add(this.lblSchemaTableName);
            this.grpExportSettings.Controls.Add(this.txtSchemaTableName);
            this.grpExportSettings.Controls.Add(this.lblExportType);
            this.grpExportSettings.Controls.Add(this.cboExportType);
            this.grpExportSettings.Controls.Add(this.lblContentType);
            this.grpExportSettings.Controls.Add(this.cboContentType);
            this.grpExportSettings.Controls.Add(this.lblCompression);
            this.grpExportSettings.Controls.Add(this.cboCompression);
            this.grpExportSettings.Controls.Add(this.lblEncryption);
            this.grpExportSettings.Controls.Add(this.cboEncryption);
            this.grpExportSettings.Controls.Add(this.lblEncryptionPassword);
            this.grpExportSettings.Controls.Add(this.txtEncryptionPassword);
            this.grpExportSettings.Controls.Add(this.lblExcludeObjects);
            this.grpExportSettings.Controls.Add(this.txtExcludeObjects);
            this.grpExportSettings.Controls.Add(this.lblIncludeObjects);
            this.grpExportSettings.Controls.Add(this.txtIncludeObjects);
            this.grpExportSettings.Controls.Add(this.chkEstimateOnly);
            this.grpExportSettings.Controls.Add(this.lblEstimateMethod);
            this.grpExportSettings.Controls.Add(this.cboEstimateMethod);
            this.grpExportSettings.Controls.Add(this.lblFlashbackTime);
            this.grpExportSettings.Controls.Add(this.txtFlashbackTime);
            this.grpExportSettings.Controls.Add(this.lblFlashbackScn);
            this.grpExportSettings.Controls.Add(this.txtFlashbackScn);
            this.grpExportSettings.Controls.Add(this.lblSamplePercentage);
            this.grpExportSettings.Controls.Add(this.numSamplePercentage);
            this.grpExportSettings.Controls.Add(this.lblStatusInterval);
            this.grpExportSettings.Controls.Add(this.numStatusInterval);
            this.grpExportSettings.Controls.Add(this.lblDataPumpJobName);
            this.grpExportSettings.Controls.Add(this.txtDataPumpJobName);
            this.grpExportSettings.Location = new System.Drawing.Point(12, 120);
            this.grpExportSettings.Name = "grpExportSettings";
            this.grpExportSettings.Size = new System.Drawing.Size(760, 400);
            this.grpExportSettings.TabIndex = 1;
            this.grpExportSettings.TabStop = false;
            this.grpExportSettings.Text = "Export Settings";
            // 
            // grpDatabaseSettings
            // 
            this.grpDatabaseSettings.Controls.Add(this.lblOracleSid);
            this.grpDatabaseSettings.Controls.Add(this.txtOracleSid);
            this.grpDatabaseSettings.Controls.Add(this.lblOracleHome);
            this.grpDatabaseSettings.Controls.Add(this.txtOracleHome);
            this.grpDatabaseSettings.Controls.Add(this.lblUsername);
            this.grpDatabaseSettings.Controls.Add(this.txtUsername);
            this.grpDatabaseSettings.Controls.Add(this.lblPassword);
            this.grpDatabaseSettings.Controls.Add(this.txtPassword);
            this.grpDatabaseSettings.Controls.Add(this.lblDatabaseName);
            this.grpDatabaseSettings.Controls.Add(this.txtDatabaseName);
            this.grpDatabaseSettings.Location = new System.Drawing.Point(12, 530);
            this.grpDatabaseSettings.Name = "grpDatabaseSettings";
            this.grpDatabaseSettings.Size = new System.Drawing.Size(760, 180);
            this.grpDatabaseSettings.TabIndex = 2;
            this.grpDatabaseSettings.TabStop = false;
            this.grpDatabaseSettings.Text = "Database Settings";
            // 
            // grpJobSettings
            // 
            this.grpJobSettings.Controls.Add(this.lblJobName);
            this.grpJobSettings.Controls.Add(this.txtJobName);
            this.grpJobSettings.Controls.Add(this.lblDescription);
            this.grpJobSettings.Controls.Add(this.txtDescription);
            this.grpJobSettings.Location = new System.Drawing.Point(12, 12);
            this.grpJobSettings.Name = "grpJobSettings";
            this.grpJobSettings.Size = new System.Drawing.Size(760, 100);
            this.grpJobSettings.TabIndex = 0;
            this.grpJobSettings.TabStop = false;
            this.grpJobSettings.Text = "Job Settings";
            // 
            // DataPumpExportForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(800, 780);
            this.Controls.Add(this.grpJobSettings);
            this.Controls.Add(this.grpExportSettings);
            this.Controls.Add(this.grpDatabaseSettings);
            this.Controls.Add(this.btnGenerateScript);
            this.Controls.Add(this.btnSave);
            this.Controls.Add(this.btnCancel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "DataPumpExportForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Oracle Data Pump Export Job Configuration";
            ((System.ComponentModel.ISupportInitialize)(this.numParallelProcesses)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numSamplePercentage)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numStatusInterval)).EndInit();
            this.grpExportSettings.ResumeLayout(false);
            this.grpExportSettings.PerformLayout();
            this.grpDatabaseSettings.ResumeLayout(false);
            this.grpDatabaseSettings.PerformLayout();
            this.grpJobSettings.ResumeLayout(false);
            this.grpJobSettings.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label lblJobName;
        private System.Windows.Forms.TextBox txtJobName;
        private System.Windows.Forms.Label lblDescription;
        private System.Windows.Forms.TextBox txtDescription;
        private System.Windows.Forms.Label lblSqlQuery;
        private System.Windows.Forms.TextBox txtSqlQuery;
        private System.Windows.Forms.Label lblOracleDirectory;
        private System.Windows.Forms.ComboBox cboOracleDirectory;
        private System.Windows.Forms.Label lblDumpFileName;
        private System.Windows.Forms.TextBox txtDumpFileName;
        private System.Windows.Forms.Label lblLogFileName;
        private System.Windows.Forms.TextBox txtLogFileName;
        private System.Windows.Forms.Label lblParallelProcesses;
        private System.Windows.Forms.NumericUpDown numParallelProcesses;
        private System.Windows.Forms.CheckBox chkFullExport;
        private System.Windows.Forms.Label lblSchemaTableName;
        private System.Windows.Forms.TextBox txtSchemaTableName;
        private System.Windows.Forms.Label lblExportType;
        private System.Windows.Forms.ComboBox cboExportType;
        private System.Windows.Forms.Label lblContentType;
        private System.Windows.Forms.ComboBox cboContentType;
        private System.Windows.Forms.Label lblCompression;
        private System.Windows.Forms.ComboBox cboCompression;
        private System.Windows.Forms.Label lblEncryption;
        private System.Windows.Forms.ComboBox cboEncryption;
        private System.Windows.Forms.Label lblEncryptionPassword;
        private System.Windows.Forms.TextBox txtEncryptionPassword;
        private System.Windows.Forms.Label lblExcludeObjects;
        private System.Windows.Forms.TextBox txtExcludeObjects;
        private System.Windows.Forms.Label lblIncludeObjects;
        private System.Windows.Forms.TextBox txtIncludeObjects;
        private System.Windows.Forms.CheckBox chkEstimateOnly;
        private System.Windows.Forms.Label lblEstimateMethod;
        private System.Windows.Forms.ComboBox cboEstimateMethod;
        private System.Windows.Forms.Label lblFlashbackTime;
        private System.Windows.Forms.TextBox txtFlashbackTime;
        private System.Windows.Forms.Label lblFlashbackScn;
        private System.Windows.Forms.TextBox txtFlashbackScn;
        private System.Windows.Forms.Label lblSamplePercentage;
        private System.Windows.Forms.NumericUpDown numSamplePercentage;
        private System.Windows.Forms.Label lblStatusInterval;
        private System.Windows.Forms.NumericUpDown numStatusInterval;
        private System.Windows.Forms.Label lblDataPumpJobName;
        private System.Windows.Forms.TextBox txtDataPumpJobName;
        private System.Windows.Forms.Label lblOracleSid;
        private System.Windows.Forms.TextBox txtOracleSid;
        private System.Windows.Forms.Label lblOracleHome;
        private System.Windows.Forms.TextBox txtOracleHome;
        private System.Windows.Forms.Label lblUsername;
        private System.Windows.Forms.TextBox txtUsername;
        private System.Windows.Forms.Label lblPassword;
        private System.Windows.Forms.TextBox txtPassword;
        private System.Windows.Forms.Label lblDatabaseName;
        private System.Windows.Forms.TextBox txtDatabaseName;
        private System.Windows.Forms.Button btnGenerateScript;
        private System.Windows.Forms.Button btnSave;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.GroupBox grpExportSettings;
        private System.Windows.Forms.GroupBox grpDatabaseSettings;
        private System.Windows.Forms.GroupBox grpJobSettings;
    }
} 