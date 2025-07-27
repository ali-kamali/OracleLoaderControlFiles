namespace ControlFileGenerator.WinForms.Forms
{
    partial class SettingsForm
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
            this.tabControl = new System.Windows.Forms.TabControl();
            this.tabTableSettings = new System.Windows.Forms.TabPage();
            this.tabFileReferences = new System.Windows.Forms.TabPage();
            this.tabAdvanced = new System.Windows.Forms.TabPage();
            this.panelButtons = new System.Windows.Forms.Panel();
            this.btnReset = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnOK = new System.Windows.Forms.Button();
            
            // Oracle Table Settings Controls
            this.lblTableName = new System.Windows.Forms.Label();
            this.txtTableName = new System.Windows.Forms.TextBox();
            this.lblLoadMode = new System.Windows.Forms.Label();
            this.cboLoadMode = new System.Windows.Forms.ComboBox();
            this.chkTrailingNullCols = new System.Windows.Forms.CheckBox();
            
            // File References Controls
            this.lblInfile = new System.Windows.Forms.Label();
            this.txtInfile = new System.Windows.Forms.TextBox();
            this.btnBrowseInfile = new System.Windows.Forms.Button();
            this.lblBadfile = new System.Windows.Forms.Label();
            this.txtBadfile = new System.Windows.Forms.TextBox();
            this.btnBrowseBadfile = new System.Windows.Forms.Button();
            this.lblDiscardfile = new System.Windows.Forms.Label();
            this.txtDiscardfile = new System.Windows.Forms.TextBox();
            this.btnBrowseDiscardfile = new System.Windows.Forms.Button();
            this.lblGlobalFileEncoding = new System.Windows.Forms.Label();
            this.cboGlobalFileEncoding = new System.Windows.Forms.ComboBox();
            this.chkUseSpecificPartition = new System.Windows.Forms.CheckBox();
            this.lblPartitionName = new System.Windows.Forms.Label();
            this.txtPartitionName = new System.Windows.Forms.TextBox();
            
            // Advanced Options Controls
            this.chkDirectPath = new System.Windows.Forms.CheckBox();
            this.lblMaxErrors = new System.Windows.Forms.Label();
            this.numMaxErrors = new System.Windows.Forms.NumericUpDown();
            this.lblBindSize = new System.Windows.Forms.Label();
            this.numBindSize = new System.Windows.Forms.NumericUpDown();
            this.lblRows = new System.Windows.Forms.Label();
            this.numRows = new System.Windows.Forms.NumericUpDown();
            
            // Field Specifications Controls
            this.lblFieldTerminator = new System.Windows.Forms.Label();
            this.txtFieldTerminator = new System.Windows.Forms.TextBox();
            this.lblEnclosedBy = new System.Windows.Forms.Label();
            this.txtEnclosedBy = new System.Windows.Forms.TextBox();
            this.chkOptionallyEnclosed = new System.Windows.Forms.CheckBox();
            this.lblTrimOption = new System.Windows.Forms.Label();
            this.cboTrimOption = new System.Windows.Forms.ComboBox();
            this.chkPreserveBlanks = new System.Windows.Forms.CheckBox();
            
            this.tabControl.SuspendLayout();
            this.tabTableSettings.SuspendLayout();
            this.tabFileReferences.SuspendLayout();
            this.tabAdvanced.SuspendLayout();
            this.panelButtons.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numMaxErrors)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numBindSize)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numRows)).BeginInit();
            this.SuspendLayout();
            
            // 
            // tabControl
            // 
            this.tabControl.Controls.Add(this.tabTableSettings);
            this.tabControl.Controls.Add(this.tabFileReferences);
            this.tabControl.Controls.Add(this.tabAdvanced);
            this.tabControl.Dock = System.Windows.Forms.DockStyle.Top;
            this.tabControl.Location = new System.Drawing.Point(0, 0);
            this.tabControl.Name = "tabControl";
            this.tabControl.SelectedIndex = 0;
            this.tabControl.Size = new System.Drawing.Size(600, 400);
            this.tabControl.TabIndex = 0;
            
            // 
            // tabTableSettings
            // 
            this.tabTableSettings.Controls.Add(this.chkTrailingNullCols);
            this.tabTableSettings.Controls.Add(this.cboLoadMode);
            this.tabTableSettings.Controls.Add(this.lblLoadMode);
            this.tabTableSettings.Controls.Add(this.txtTableName);
            this.tabTableSettings.Controls.Add(this.lblTableName);
            this.tabTableSettings.Location = new System.Drawing.Point(4, 24);
            this.tabTableSettings.Name = "tabTableSettings";
            this.tabTableSettings.Padding = new System.Windows.Forms.Padding(3);
            this.tabTableSettings.Size = new System.Drawing.Size(592, 372);
            this.tabTableSettings.TabIndex = 0;
            this.tabTableSettings.Text = "Table Settings";
            this.tabTableSettings.UseVisualStyleBackColor = true;
            
            // 
            // tabFileReferences
            // 
            this.tabFileReferences.Controls.Add(this.txtPartitionName);
            this.tabFileReferences.Controls.Add(this.lblPartitionName);
            this.tabFileReferences.Controls.Add(this.chkUseSpecificPartition);
            this.tabFileReferences.Controls.Add(this.cboGlobalFileEncoding);
            this.tabFileReferences.Controls.Add(this.lblGlobalFileEncoding);
            this.tabFileReferences.Controls.Add(this.btnBrowseDiscardfile);
            this.tabFileReferences.Controls.Add(this.txtDiscardfile);
            this.tabFileReferences.Controls.Add(this.lblDiscardfile);
            this.tabFileReferences.Controls.Add(this.btnBrowseBadfile);
            this.tabFileReferences.Controls.Add(this.txtBadfile);
            this.tabFileReferences.Controls.Add(this.lblBadfile);
            this.tabFileReferences.Controls.Add(this.btnBrowseInfile);
            this.tabFileReferences.Controls.Add(this.txtInfile);
            this.tabFileReferences.Controls.Add(this.lblInfile);
            this.tabFileReferences.Location = new System.Drawing.Point(4, 24);
            this.tabFileReferences.Name = "tabFileReferences";
            this.tabFileReferences.Padding = new System.Windows.Forms.Padding(3);
            this.tabFileReferences.Size = new System.Drawing.Size(592, 372);
            this.tabFileReferences.TabIndex = 1;
            this.tabFileReferences.Text = "File References";
            this.tabFileReferences.UseVisualStyleBackColor = true;
            
            // 
            // tabAdvanced
            // 
            this.tabAdvanced.Controls.Add(this.chkPreserveBlanks);
            this.tabAdvanced.Controls.Add(this.cboTrimOption);
            this.tabAdvanced.Controls.Add(this.lblTrimOption);
            this.tabAdvanced.Controls.Add(this.chkOptionallyEnclosed);
            this.tabAdvanced.Controls.Add(this.txtEnclosedBy);
            this.tabAdvanced.Controls.Add(this.lblEnclosedBy);
            this.tabAdvanced.Controls.Add(this.txtFieldTerminator);
            this.tabAdvanced.Controls.Add(this.lblFieldTerminator);
            this.tabAdvanced.Controls.Add(this.numRows);
            this.tabAdvanced.Controls.Add(this.lblRows);
            this.tabAdvanced.Controls.Add(this.numBindSize);
            this.tabAdvanced.Controls.Add(this.lblBindSize);
            this.tabAdvanced.Controls.Add(this.numMaxErrors);
            this.tabAdvanced.Controls.Add(this.lblMaxErrors);
            this.tabAdvanced.Controls.Add(this.chkDirectPath);
            this.tabAdvanced.Location = new System.Drawing.Point(4, 24);
            this.tabAdvanced.Name = "tabAdvanced";
            this.tabAdvanced.Padding = new System.Windows.Forms.Padding(3);
            this.tabAdvanced.Size = new System.Drawing.Size(592, 372);
            this.tabAdvanced.TabIndex = 2;
            this.tabAdvanced.Text = "Advanced Options";
            this.tabAdvanced.UseVisualStyleBackColor = true;
            
            // 
            // panelButtons
            // 
            this.panelButtons.Controls.Add(this.btnReset);
            this.panelButtons.Controls.Add(this.btnCancel);
            this.panelButtons.Controls.Add(this.btnOK);
            this.panelButtons.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panelButtons.Location = new System.Drawing.Point(0, 400);
            this.panelButtons.Name = "panelButtons";
            this.panelButtons.Size = new System.Drawing.Size(600, 50);
            this.panelButtons.TabIndex = 1;
            
            // 
            // Oracle Table Settings Controls
            // 
            this.lblTableName.AutoSize = true;
            this.lblTableName.Location = new System.Drawing.Point(20, 20);
            this.lblTableName.Name = "lblTableName";
            this.lblTableName.Size = new System.Drawing.Size(80, 17);
            this.lblTableName.TabIndex = 0;
            this.lblTableName.Text = "Table Name:";
            
            this.txtTableName.Location = new System.Drawing.Point(120, 17);
            this.txtTableName.Name = "txtTableName";
            this.txtTableName.Size = new System.Drawing.Size(200, 23);
            this.txtTableName.TabIndex = 1;
            
            this.lblLoadMode.AutoSize = true;
            this.lblLoadMode.Location = new System.Drawing.Point(20, 60);
            this.lblLoadMode.Name = "lblLoadMode";
            this.lblLoadMode.Size = new System.Drawing.Size(80, 17);
            this.lblLoadMode.TabIndex = 2;
            this.lblLoadMode.Text = "Load Mode:";
            
            this.cboLoadMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboLoadMode.FormattingEnabled = true;
            this.cboLoadMode.Items.AddRange(new object[] {
                "APPEND",
                "REPLACE",
                "INSERT",
                "TRUNCATE"});
            this.cboLoadMode.Location = new System.Drawing.Point(120, 57);
            this.cboLoadMode.Name = "cboLoadMode";
            this.cboLoadMode.Size = new System.Drawing.Size(200, 25);
            this.cboLoadMode.TabIndex = 3;
            
            this.chkTrailingNullCols.AutoSize = true;
            this.chkTrailingNullCols.Location = new System.Drawing.Point(120, 100);
            this.chkTrailingNullCols.Name = "chkTrailingNullCols";
            this.chkTrailingNullCols.Size = new System.Drawing.Size(150, 21);
            this.chkTrailingNullCols.TabIndex = 4;
            this.chkTrailingNullCols.Text = "TRAILING NULLCOLS";
            this.chkTrailingNullCols.UseVisualStyleBackColor = true;
            
            // 
            // File References Controls
            // 
            this.lblInfile.AutoSize = true;
            this.lblInfile.Location = new System.Drawing.Point(20, 20);
            this.lblInfile.Name = "lblInfile";
            this.lblInfile.Size = new System.Drawing.Size(80, 17);
            this.lblInfile.TabIndex = 0;
            this.lblInfile.Text = "Input File:";
            
            this.txtInfile.Location = new System.Drawing.Point(120, 17);
            this.txtInfile.Name = "txtInfile";
            this.txtInfile.Size = new System.Drawing.Size(300, 23);
            this.txtInfile.TabIndex = 1;
            
            this.btnBrowseInfile.Location = new System.Drawing.Point(430, 16);
            this.btnBrowseInfile.Name = "btnBrowseInfile";
            this.btnBrowseInfile.Size = new System.Drawing.Size(75, 25);
            this.btnBrowseInfile.TabIndex = 2;
            this.btnBrowseInfile.Text = "Browse";
            this.btnBrowseInfile.UseVisualStyleBackColor = true;
            this.btnBrowseInfile.Click += new System.EventHandler(this.btnBrowseInfile_Click);
            
            this.lblBadfile.AutoSize = true;
            this.lblBadfile.Location = new System.Drawing.Point(20, 60);
            this.lblBadfile.Name = "lblBadfile";
            this.lblBadfile.Size = new System.Drawing.Size(80, 17);
            this.lblBadfile.TabIndex = 3;
            this.lblBadfile.Text = "Bad File:";
            
            this.txtBadfile.Location = new System.Drawing.Point(120, 57);
            this.txtBadfile.Name = "txtBadfile";
            this.txtBadfile.Size = new System.Drawing.Size(300, 23);
            this.txtBadfile.TabIndex = 4;
            
            this.btnBrowseBadfile.Location = new System.Drawing.Point(430, 56);
            this.btnBrowseBadfile.Name = "btnBrowseBadfile";
            this.btnBrowseBadfile.Size = new System.Drawing.Size(75, 25);
            this.btnBrowseBadfile.TabIndex = 5;
            this.btnBrowseBadfile.Text = "Browse";
            this.btnBrowseBadfile.UseVisualStyleBackColor = true;
            this.btnBrowseBadfile.Click += new System.EventHandler(this.btnBrowseBadfile_Click);
            
            this.lblDiscardfile.AutoSize = true;
            this.lblDiscardfile.Location = new System.Drawing.Point(20, 100);
            this.lblDiscardfile.Name = "lblDiscardfile";
            this.lblDiscardfile.Size = new System.Drawing.Size(80, 17);
            this.lblDiscardfile.TabIndex = 6;
            this.lblDiscardfile.Text = "Discard File:";
            
            this.txtDiscardfile.Location = new System.Drawing.Point(120, 97);
            this.txtDiscardfile.Name = "txtDiscardfile";
            this.txtDiscardfile.Size = new System.Drawing.Size(300, 23);
            this.txtDiscardfile.TabIndex = 7;
            
            this.btnBrowseDiscardfile.Location = new System.Drawing.Point(430, 96);
            this.btnBrowseDiscardfile.Name = "btnBrowseDiscardfile";
            this.btnBrowseDiscardfile.Size = new System.Drawing.Size(75, 25);
            this.btnBrowseDiscardfile.TabIndex = 8;
            this.btnBrowseDiscardfile.Text = "Browse";
            this.btnBrowseDiscardfile.UseVisualStyleBackColor = true;
            this.btnBrowseDiscardfile.Click += new System.EventHandler(this.btnBrowseDiscardfile_Click);
            

            
            this.lblGlobalFileEncoding.AutoSize = true;
            this.lblGlobalFileEncoding.Location = new System.Drawing.Point(20, 180);
            this.lblGlobalFileEncoding.Name = "lblGlobalFileEncoding";
            this.lblGlobalFileEncoding.Size = new System.Drawing.Size(120, 17);
            this.lblGlobalFileEncoding.TabIndex = 11;
            this.lblGlobalFileEncoding.Text = "Global File Encoding:";
            
            this.cboGlobalFileEncoding.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboGlobalFileEncoding.FormattingEnabled = true;
            this.cboGlobalFileEncoding.Items.AddRange(new object[] {
                "UTF8",
                "WE8ISO8859P1",
                "AL32UTF8",
                "JA16SJIS",
                "ZHS16GBK",
                "KO16MSWIN949"});
            this.cboGlobalFileEncoding.Location = new System.Drawing.Point(150, 177);
            this.cboGlobalFileEncoding.Name = "cboGlobalFileEncoding";
            this.cboGlobalFileEncoding.Size = new System.Drawing.Size(170, 25);
            this.cboGlobalFileEncoding.TabIndex = 12;
            
            this.chkUseSpecificPartition.AutoSize = true;
            this.chkUseSpecificPartition.Location = new System.Drawing.Point(20, 220);
            this.chkUseSpecificPartition.Name = "chkUseSpecificPartition";
            this.chkUseSpecificPartition.Size = new System.Drawing.Size(200, 21);
            this.chkUseSpecificPartition.TabIndex = 13;
            this.chkUseSpecificPartition.Text = "Load into specific partition (advanced)";
            this.chkUseSpecificPartition.UseVisualStyleBackColor = true;
            this.chkUseSpecificPartition.CheckedChanged += new System.EventHandler(this.chkUseSpecificPartition_CheckedChanged);
            
            this.lblPartitionName.AutoSize = true;
            this.lblPartitionName.Location = new System.Drawing.Point(40, 250);
            this.lblPartitionName.Name = "lblPartitionName";
            this.lblPartitionName.Size = new System.Drawing.Size(100, 17);
            this.lblPartitionName.TabIndex = 14;
            this.lblPartitionName.Text = "Partition Name:";
            this.lblPartitionName.Enabled = false;
            
            this.txtPartitionName.Location = new System.Drawing.Point(150, 247);
            this.txtPartitionName.Name = "txtPartitionName";
            this.txtPartitionName.Size = new System.Drawing.Size(200, 23);
            this.txtPartitionName.TabIndex = 15;
            this.txtPartitionName.Enabled = false;
            
            // 
            // Advanced Options Controls
            // 
            this.chkDirectPath.AutoSize = true;
            this.chkDirectPath.Location = new System.Drawing.Point(20, 20);
            this.chkDirectPath.Name = "chkDirectPath";
            this.chkDirectPath.Size = new System.Drawing.Size(150, 21);
            this.chkDirectPath.TabIndex = 0;
            this.chkDirectPath.Text = "DIRECT=TRUE";
            this.chkDirectPath.UseVisualStyleBackColor = true;
            
            this.lblMaxErrors.AutoSize = true;
            this.lblMaxErrors.Location = new System.Drawing.Point(20, 60);
            this.lblMaxErrors.Name = "lblMaxErrors";
            this.lblMaxErrors.Size = new System.Drawing.Size(80, 17);
            this.lblMaxErrors.TabIndex = 1;
            this.lblMaxErrors.Text = "Max Errors:";
            
            this.numMaxErrors.Location = new System.Drawing.Point(120, 57);
            this.numMaxErrors.Maximum = new decimal(new int[] { 999999, 0, 0, 0 });
            this.numMaxErrors.Minimum = new decimal(new int[] { 0, 0, 0, 0 });
            this.numMaxErrors.Name = "numMaxErrors";
            this.numMaxErrors.Size = new System.Drawing.Size(100, 23);
            this.numMaxErrors.TabIndex = 2;
            this.numMaxErrors.Value = new decimal(new int[] { 50, 0, 0, 0 });
            
            this.lblBindSize.AutoSize = true;
            this.lblBindSize.Location = new System.Drawing.Point(20, 100);
            this.lblBindSize.Name = "lblBindSize";
            this.lblBindSize.Size = new System.Drawing.Size(80, 17);
            this.lblBindSize.TabIndex = 3;
            this.lblBindSize.Text = "Bind Size:";
            
            this.numBindSize.Location = new System.Drawing.Point(120, 97);
            this.numBindSize.Maximum = new decimal(new int[] { 999999999, 0, 0, 0 });
            this.numBindSize.Minimum = new decimal(new int[] { 1, 0, 0, 0 });
            this.numBindSize.Name = "numBindSize";
            this.numBindSize.Size = new System.Drawing.Size(100, 23);
            this.numBindSize.TabIndex = 4;
            this.numBindSize.Value = new decimal(new int[] { 1048576, 0, 0, 0 });
            
            this.lblRows.AutoSize = true;
            this.lblRows.Location = new System.Drawing.Point(20, 140);
            this.lblRows.Name = "lblRows";
            this.lblRows.Size = new System.Drawing.Size(80, 17);
            this.lblRows.TabIndex = 5;
            this.lblRows.Text = "Rows:";
            
            this.numRows.Location = new System.Drawing.Point(120, 137);
            this.numRows.Maximum = new decimal(new int[] { 999999999, 0, 0, 0 });
            this.numRows.Minimum = new decimal(new int[] { 1, 0, 0, 0 });
            this.numRows.Name = "numRows";
            this.numRows.Size = new System.Drawing.Size(100, 23);
            this.numRows.TabIndex = 6;
            this.numRows.Value = new decimal(new int[] { 50000, 0, 0, 0 });
            
            // Field Specifications Controls
            this.lblFieldTerminator.AutoSize = true;
            this.lblFieldTerminator.Location = new System.Drawing.Point(20, 180);
            this.lblFieldTerminator.Name = "lblFieldTerminator";
            this.lblFieldTerminator.Size = new System.Drawing.Size(100, 17);
            this.lblFieldTerminator.TabIndex = 7;
            this.lblFieldTerminator.Text = "Field Terminator:";
            
            this.txtFieldTerminator.Location = new System.Drawing.Point(120, 177);
            this.txtFieldTerminator.Name = "txtFieldTerminator";
            this.txtFieldTerminator.Size = new System.Drawing.Size(50, 23);
            this.txtFieldTerminator.TabIndex = 8;
            this.txtFieldTerminator.Text = ",";
            
            this.lblEnclosedBy.AutoSize = true;
            this.lblEnclosedBy.Location = new System.Drawing.Point(20, 220);
            this.lblEnclosedBy.Name = "lblEnclosedBy";
            this.lblEnclosedBy.Size = new System.Drawing.Size(100, 17);
            this.lblEnclosedBy.TabIndex = 9;
            this.lblEnclosedBy.Text = "Enclosed By:";
            
            this.txtEnclosedBy.Location = new System.Drawing.Point(120, 217);
            this.txtEnclosedBy.Name = "txtEnclosedBy";
            this.txtEnclosedBy.Size = new System.Drawing.Size(50, 23);
            this.txtEnclosedBy.TabIndex = 10;
            this.txtEnclosedBy.Text = "\"";
            
            this.chkOptionallyEnclosed.AutoSize = true;
            this.chkOptionallyEnclosed.Location = new System.Drawing.Point(180, 220);
            this.chkOptionallyEnclosed.Name = "chkOptionallyEnclosed";
            this.chkOptionallyEnclosed.Size = new System.Drawing.Size(150, 21);
            this.chkOptionallyEnclosed.TabIndex = 11;
            this.chkOptionallyEnclosed.Text = "Optionally Enclosed";
            this.chkOptionallyEnclosed.UseVisualStyleBackColor = true;
            
            this.lblTrimOption.AutoSize = true;
            this.lblTrimOption.Location = new System.Drawing.Point(20, 260);
            this.lblTrimOption.Name = "lblTrimOption";
            this.lblTrimOption.Size = new System.Drawing.Size(100, 17);
            this.lblTrimOption.TabIndex = 12;
            this.lblTrimOption.Text = "Trim Option:";
            
            this.cboTrimOption.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboTrimOption.FormattingEnabled = true;
            this.cboTrimOption.Items.AddRange(new object[] {
                "LRTRIM",
                "LTRIM",
                "RTRIM",
                "NOTRIM"});
            this.cboTrimOption.Location = new System.Drawing.Point(120, 257);
            this.cboTrimOption.Name = "cboTrimOption";
            this.cboTrimOption.Size = new System.Drawing.Size(150, 25);
            this.cboTrimOption.TabIndex = 13;
            
            this.chkPreserveBlanks.AutoSize = true;
            this.chkPreserveBlanks.Location = new System.Drawing.Point(20, 300);
            this.chkPreserveBlanks.Name = "chkPreserveBlanks";
            this.chkPreserveBlanks.Size = new System.Drawing.Size(150, 21);
            this.chkPreserveBlanks.TabIndex = 14;
            this.chkPreserveBlanks.Text = "Preserve Blanks";
            this.chkPreserveBlanks.UseVisualStyleBackColor = true;
            
            // 
            // Button Controls
            // 
            this.btnOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnOK.Location = new System.Drawing.Point(420, 12);
            this.btnOK.Name = "btnOK";
            this.btnOK.Size = new System.Drawing.Size(75, 25);
            this.btnOK.TabIndex = 0;
            this.btnOK.Text = "OK";
            this.btnOK.UseVisualStyleBackColor = true;
            this.btnOK.Click += new System.EventHandler(this.btnOK_Click);
            
            this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCancel.Location = new System.Drawing.Point(510, 12);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(75, 25);
            this.btnCancel.TabIndex = 1;
            this.btnCancel.Text = "Cancel";
            this.btnCancel.UseVisualStyleBackColor = true;
            this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
            
            this.btnReset.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.btnReset.Location = new System.Drawing.Point(12, 12);
            this.btnReset.Name = "btnReset";
            this.btnReset.Size = new System.Drawing.Size(75, 25);
            this.btnReset.TabIndex = 2;
            this.btnReset.Text = "Reset";
            this.btnReset.UseVisualStyleBackColor = true;
            this.btnReset.Click += new System.EventHandler(this.btnReset_Click);
            
            // 
            // SettingsForm
            // 
            this.AcceptButton = this.btnOK;
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 17F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(600, 450);
            this.Controls.Add(this.panelButtons);
            this.Controls.Add(this.tabControl);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "SettingsForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Loader Settings";
            this.tabControl.ResumeLayout(false);
            this.tabTableSettings.ResumeLayout(false);
            this.tabTableSettings.PerformLayout();
            this.tabFileReferences.ResumeLayout(false);
            this.tabFileReferences.PerformLayout();
            this.tabAdvanced.ResumeLayout(false);
            this.tabAdvanced.PerformLayout();
            this.panelButtons.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.numMaxErrors)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numBindSize)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numRows)).EndInit();
            this.ResumeLayout(false);
        }

        #endregion

        private System.Windows.Forms.TabControl tabControl;
        private System.Windows.Forms.TabPage tabTableSettings;
        private System.Windows.Forms.TabPage tabFileReferences;
        private System.Windows.Forms.TabPage tabAdvanced;
        private System.Windows.Forms.Panel panelButtons;
        private System.Windows.Forms.Button btnOK;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnReset;
        
        // Oracle Table Settings Controls
        private System.Windows.Forms.Label lblTableName;
        private System.Windows.Forms.TextBox txtTableName;
        private System.Windows.Forms.Label lblLoadMode;
        private System.Windows.Forms.ComboBox cboLoadMode;
        private System.Windows.Forms.CheckBox chkTrailingNullCols;
        
        // File References Controls
        private System.Windows.Forms.Label lblInfile;
        private System.Windows.Forms.TextBox txtInfile;
        private System.Windows.Forms.Button btnBrowseInfile;
        private System.Windows.Forms.Label lblBadfile;
        private System.Windows.Forms.TextBox txtBadfile;
        private System.Windows.Forms.Button btnBrowseBadfile;
        private System.Windows.Forms.Label lblDiscardfile;
        private System.Windows.Forms.TextBox txtDiscardfile;
        private System.Windows.Forms.Button btnBrowseDiscardfile;
        private System.Windows.Forms.Label lblGlobalFileEncoding;
        private System.Windows.Forms.ComboBox cboGlobalFileEncoding;
        private System.Windows.Forms.CheckBox chkUseSpecificPartition;
        private System.Windows.Forms.Label lblPartitionName;
        private System.Windows.Forms.TextBox txtPartitionName;
        
        // Advanced Options Controls
        private System.Windows.Forms.CheckBox chkDirectPath;
        private System.Windows.Forms.Label lblMaxErrors;
        private System.Windows.Forms.NumericUpDown numMaxErrors;
        private System.Windows.Forms.Label lblBindSize;
        private System.Windows.Forms.NumericUpDown numBindSize;
        private System.Windows.Forms.Label lblRows;
        private System.Windows.Forms.NumericUpDown numRows;
        
        // Field Specifications Controls
        private System.Windows.Forms.Label lblFieldTerminator;
        private System.Windows.Forms.TextBox txtFieldTerminator;
        private System.Windows.Forms.Label lblEnclosedBy;
        private System.Windows.Forms.TextBox txtEnclosedBy;
        private System.Windows.Forms.CheckBox chkOptionallyEnclosed;
        private System.Windows.Forms.Label lblTrimOption;
        private System.Windows.Forms.ComboBox cboTrimOption;
        private System.Windows.Forms.CheckBox chkPreserveBlanks;
    }
} 