namespace ControlFileGenerator.WinForms.Forms
{
    partial class MainForm
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        ///  Clean up any resources being used.
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
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.btnLoadExcel = new System.Windows.Forms.Button();
            this.cboSheet = new System.Windows.Forms.ComboBox();
            this.dgvFields = new System.Windows.Forms.DataGridView();
            this.btnSettings = new System.Windows.Forms.Button();
            this.btnPreview = new System.Windows.Forms.Button();
            this.btnExport = new System.Windows.Forms.Button();
            this.btnDataPreview = new System.Windows.Forms.Button();
            this.lblStatus = new System.Windows.Forms.Label();
            this.lblSheet = new System.Windows.Forms.Label();
            this.panelTop = new System.Windows.Forms.Panel();
            this.panelBottom = new System.Windows.Forms.Panel();
            this.panelMain = new System.Windows.Forms.Panel();
            this.btnStartFromScratch = new System.Windows.Forms.Button();
            this.btnAddField = new System.Windows.Forms.Button();
            this.btnRemoveField = new System.Windows.Forms.Button();
            this.btnToggleMode = new System.Windows.Forms.Button();
            this.btnValidate = new System.Windows.Forms.Button();
            this.btnAutoFix = new System.Windows.Forms.Button();
            this.lblMode = new System.Windows.Forms.Label();
            this.panelToolbar = new System.Windows.Forms.Panel();
            this.panelStatus = new System.Windows.Forms.Panel();
            ((System.ComponentModel.ISupportInitialize)(this.dgvFields)).BeginInit();
            this.panelTop.SuspendLayout();
            this.panelBottom.SuspendLayout();
            this.panelMain.SuspendLayout();
            this.panelToolbar.SuspendLayout();
            this.panelStatus.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnLoadExcel
            // 
            this.btnLoadExcel.Location = new System.Drawing.Point(12, 12);
            this.btnLoadExcel.Name = "btnLoadExcel";
            this.btnLoadExcel.Size = new System.Drawing.Size(140, 30);
            this.btnLoadExcel.TabIndex = 0;
            this.btnLoadExcel.Text = "Load Excel Metadata";
            this.btnLoadExcel.UseVisualStyleBackColor = true;
            // 
            // cboSheet
            // 
            this.cboSheet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboSheet.Enabled = false;
            this.cboSheet.FormattingEnabled = true;
            this.cboSheet.Location = new System.Drawing.Point(220, 15);
            this.cboSheet.Name = "cboSheet";
            this.cboSheet.Size = new System.Drawing.Size(200, 25);
            this.cboSheet.TabIndex = 1;
            // 
            // dgvFields
            // 
            this.dgvFields.AllowUserToAddRows = false;
            this.dgvFields.AllowUserToDeleteRows = false;
            this.dgvFields.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgvFields.Dock = System.Windows.Forms.DockStyle.Fill;
            this.dgvFields.Location = new System.Drawing.Point(0, 0);
            this.dgvFields.Name = "dgvFields";
            this.dgvFields.RowTemplate.Height = 25;
            this.dgvFields.Size = new System.Drawing.Size(1164, 580);
            this.dgvFields.TabIndex = 2;
            // 
            // btnSettings
            // 
            this.btnSettings.Location = new System.Drawing.Point(12, 8);
            this.btnSettings.Name = "btnSettings";
            this.btnSettings.Size = new System.Drawing.Size(100, 30);
            this.btnSettings.TabIndex = 3;
            this.btnSettings.Text = "Settings";
            this.btnSettings.UseVisualStyleBackColor = true;
            // 
            // btnPreview
            // 
            this.btnPreview.Enabled = false;
            this.btnPreview.Location = new System.Drawing.Point(118, 8);
            this.btnPreview.Name = "btnPreview";
            this.btnPreview.Size = new System.Drawing.Size(100, 30);
            this.btnPreview.TabIndex = 4;
            this.btnPreview.Text = "Preview";
            this.btnPreview.UseVisualStyleBackColor = true;
            // 
            // btnExport
            // 
            this.btnExport.Enabled = false;
            this.btnExport.Location = new System.Drawing.Point(224, 8);
            this.btnExport.Name = "btnExport";
            this.btnExport.Size = new System.Drawing.Size(100, 30);
            this.btnExport.TabIndex = 5;
            this.btnExport.Text = "Export .ctl";
            this.btnExport.UseVisualStyleBackColor = true;
            // 
            // btnDataPreview
            // 
            this.btnDataPreview.Enabled = false;
            this.btnDataPreview.Location = new System.Drawing.Point(330, 8);
            this.btnDataPreview.Name = "btnDataPreview";
            this.btnDataPreview.Size = new System.Drawing.Size(100, 30);
            this.btnDataPreview.TabIndex = 6;
            this.btnDataPreview.Text = "Data Preview";
            this.btnDataPreview.UseVisualStyleBackColor = true;
            // 
            // lblStatus
            // 
            this.lblStatus.AutoSize = true;
            this.lblStatus.Location = new System.Drawing.Point(12, 15);
            this.lblStatus.Name = "lblStatus";
            this.lblStatus.Size = new System.Drawing.Size(100, 17);
            this.lblStatus.TabIndex = 6;
            this.lblStatus.Text = "Ready";
            // 
            // lblSheet
            // 
            this.lblSheet.AutoSize = true;
            this.lblSheet.Location = new System.Drawing.Point(170, 18);
            this.lblSheet.Name = "lblSheet";
            this.lblSheet.Size = new System.Drawing.Size(44, 17);
            this.lblSheet.TabIndex = 7;
            this.lblSheet.Text = "Sheet:";
            // 
            // panelTop
            // 
            this.panelTop.Controls.Add(this.lblSheet);
            this.panelTop.Controls.Add(this.cboSheet);
            this.panelTop.Controls.Add(this.btnLoadExcel);
            this.panelTop.Dock = System.Windows.Forms.DockStyle.Top;
            this.panelTop.Location = new System.Drawing.Point(0, 0);
            this.panelTop.Name = "panelTop";
            this.panelTop.Size = new System.Drawing.Size(1164, 60);
            this.panelTop.TabIndex = 8;
            // 
            // panelBottom
            // 
            this.panelBottom.Controls.Add(this.panelStatus);
            this.panelBottom.Controls.Add(this.panelToolbar);
            this.panelBottom.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panelBottom.Location = new System.Drawing.Point(0, 640);
            this.panelBottom.Name = "panelBottom";
            this.panelBottom.Size = new System.Drawing.Size(1164, 120);
            this.panelBottom.TabIndex = 9;
            // 
            // panelMain
            // 
            this.panelMain.Controls.Add(this.dgvFields);
            this.panelMain.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panelMain.Location = new System.Drawing.Point(0, 60);
            this.panelMain.Name = "panelMain";
            this.panelMain.Size = new System.Drawing.Size(1164, 580);
            this.panelMain.TabIndex = 10;
            // 
            // btnStartFromScratch
            // 
            this.btnStartFromScratch.Location = new System.Drawing.Point(12, 8);
            this.btnStartFromScratch.Name = "btnStartFromScratch";
            this.btnStartFromScratch.Size = new System.Drawing.Size(120, 30);
            this.btnStartFromScratch.TabIndex = 11;
            this.btnStartFromScratch.Text = "Start from Scratch";
            this.btnStartFromScratch.UseVisualStyleBackColor = true;
            // 
            // btnAddField
            // 
            this.btnAddField.Location = new System.Drawing.Point(138, 8);
            this.btnAddField.Name = "btnAddField";
            this.btnAddField.Size = new System.Drawing.Size(80, 30);
            this.btnAddField.TabIndex = 12;
            this.btnAddField.Text = "Add Field";
            this.btnAddField.UseVisualStyleBackColor = true;
            // 
            // btnRemoveField
            // 
            this.btnRemoveField.Location = new System.Drawing.Point(224, 8);
            this.btnRemoveField.Name = "btnRemoveField";
            this.btnRemoveField.Size = new System.Drawing.Size(90, 30);
            this.btnRemoveField.TabIndex = 13;
            this.btnRemoveField.Text = "Remove Field";
            this.btnRemoveField.UseVisualStyleBackColor = true;
            // 
            // btnToggleMode
            // 
            this.btnToggleMode.Location = new System.Drawing.Point(320, 8);
            this.btnToggleMode.Name = "btnToggleMode";
            this.btnToggleMode.Size = new System.Drawing.Size(100, 30);
            this.btnToggleMode.TabIndex = 14;
            this.btnToggleMode.Text = "Toggle Mode";
            this.btnToggleMode.UseVisualStyleBackColor = true;
            // 
            // btnValidate
            // 
            this.btnValidate.Location = new System.Drawing.Point(426, 8);
            this.btnValidate.Name = "btnValidate";
            this.btnValidate.Size = new System.Drawing.Size(80, 30);
            this.btnValidate.TabIndex = 15;
            this.btnValidate.Text = "Validate";
            this.btnValidate.UseVisualStyleBackColor = true;
            // 
            // btnAutoFix
            // 
            this.btnAutoFix.Location = new System.Drawing.Point(512, 8);
            this.btnAutoFix.Name = "btnAutoFix";
            this.btnAutoFix.Size = new System.Drawing.Size(80, 30);
            this.btnAutoFix.TabIndex = 16;
            this.btnAutoFix.Text = "Auto Fix";
            this.btnAutoFix.UseVisualStyleBackColor = true;
            // 
            // lblMode
            // 
            this.lblMode.AutoSize = true;
            this.lblMode.Font = new System.Drawing.Font("Segoe UI", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.lblMode.Location = new System.Drawing.Point(600, 15);
            this.lblMode.Name = "lblMode";
            this.lblMode.Size = new System.Drawing.Size(89, 15);
            this.lblMode.TabIndex = 17;
            this.lblMode.Text = "Fixed Width";
            // 
            // panelToolbar
            // 
            this.panelToolbar.Controls.Add(this.lblMode);
            this.panelToolbar.Controls.Add(this.btnAutoFix);
            this.panelToolbar.Controls.Add(this.btnValidate);
            this.panelToolbar.Controls.Add(this.btnToggleMode);
            this.panelToolbar.Controls.Add(this.btnRemoveField);
            this.panelToolbar.Controls.Add(this.btnAddField);
            this.panelToolbar.Controls.Add(this.btnStartFromScratch);
            this.panelToolbar.Controls.Add(this.btnSettings);
            this.panelToolbar.Controls.Add(this.btnPreview);
            this.panelToolbar.Controls.Add(this.btnExport);
            this.panelToolbar.Controls.Add(this.btnDataPreview);
            this.panelToolbar.Dock = System.Windows.Forms.DockStyle.Top;
            this.panelToolbar.Location = new System.Drawing.Point(0, 0);
            this.panelToolbar.Name = "panelToolbar";
            this.panelToolbar.Size = new System.Drawing.Size(1164, 50);
            this.panelToolbar.TabIndex = 18;
            // 
            // panelStatus
            // 
            this.panelStatus.Controls.Add(this.lblStatus);
            this.panelStatus.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panelStatus.Location = new System.Drawing.Point(0, 50);
            this.panelStatus.Name = "panelStatus";
            this.panelStatus.Size = new System.Drawing.Size(1164, 70);
            this.panelStatus.TabIndex = 19;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 17F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1164, 760);
            this.Controls.Add(this.panelMain);
            this.Controls.Add(this.panelBottom);
            this.Controls.Add(this.panelTop);
            this.MinimumSize = new System.Drawing.Size(800, 600);
            this.Name = "MainForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Oracle SQL*Loader Control File Generator";
            ((System.ComponentModel.ISupportInitialize)(this.dgvFields)).EndInit();
            this.panelTop.ResumeLayout(false);
            this.panelTop.PerformLayout();
            this.panelBottom.ResumeLayout(false);
            this.panelMain.ResumeLayout(false);
            this.panelToolbar.ResumeLayout(false);
            this.panelToolbar.PerformLayout();
            this.panelStatus.ResumeLayout(false);
            this.panelStatus.PerformLayout();
            this.ResumeLayout(false);
        }

        #endregion

        private System.Windows.Forms.Button btnLoadExcel;
        private System.Windows.Forms.ComboBox cboSheet;
        private System.Windows.Forms.DataGridView dgvFields;
        private System.Windows.Forms.Button btnSettings;
        private System.Windows.Forms.Button btnPreview;
        private System.Windows.Forms.Button btnExport;
        private System.Windows.Forms.Button btnDataPreview;
        private System.Windows.Forms.Label lblStatus;
        private System.Windows.Forms.Label lblSheet;
        private System.Windows.Forms.Panel panelTop;
        private System.Windows.Forms.Panel panelBottom;
        private System.Windows.Forms.Panel panelMain;
        private System.Windows.Forms.Button btnStartFromScratch;
        private System.Windows.Forms.Button btnAddField;
        private System.Windows.Forms.Button btnRemoveField;
        private System.Windows.Forms.Button btnToggleMode;
        private System.Windows.Forms.Button btnValidate;
        private System.Windows.Forms.Button btnAutoFix;
        private System.Windows.Forms.Label lblMode;
        private System.Windows.Forms.Panel panelToolbar;
        private System.Windows.Forms.Panel panelStatus;
    }
} 