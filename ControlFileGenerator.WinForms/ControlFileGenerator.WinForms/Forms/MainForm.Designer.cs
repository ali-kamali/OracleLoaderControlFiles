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
            btnLoadExcel = new Button();
            cboSheet = new ComboBox();
            dgvFields = new DataGridView();
            btnSettings = new Button();
            btnPreview = new Button();
            btnExport = new Button();
            btnDataPreview = new Button();
            btnGenerateTable = new Button();
            lblStatus = new Label();
            lblSheet = new Label();
            panelTop = new Panel();
            panelBottom = new Panel();
            panelStatus = new Panel();
            panelToolbar = new Panel();
            lblMode = new Label();
            btnAutoFix = new Button();
            btnValidate = new Button();
            btnToggleMode = new Button();
            btnRemoveField = new Button();
            btnAddField = new Button();
            btnStartFromScratch = new Button();
            panelMain = new Panel();
            ((System.ComponentModel.ISupportInitialize)dgvFields).BeginInit();
            panelTop.SuspendLayout();
            panelBottom.SuspendLayout();
            panelStatus.SuspendLayout();
            panelToolbar.SuspendLayout();
            panelMain.SuspendLayout();
            SuspendLayout();
            // 
            // btnLoadExcel
            // 
            btnLoadExcel.Location = new Point(14, 14);
            btnLoadExcel.Margin = new Padding(3, 4, 3, 4);
            btnLoadExcel.Name = "btnLoadExcel";
            btnLoadExcel.Size = new Size(160, 35);
            btnLoadExcel.TabIndex = 0;
            btnLoadExcel.Text = "Load Excel Metadata";
            btnLoadExcel.UseVisualStyleBackColor = true;
            // 
            // cboSheet
            // 
            cboSheet.DropDownStyle = ComboBoxStyle.DropDownList;
            cboSheet.Enabled = false;
            cboSheet.FormattingEnabled = true;
            cboSheet.Location = new Point(251, 18);
            cboSheet.Margin = new Padding(3, 4, 3, 4);
            cboSheet.Name = "cboSheet";
            cboSheet.Size = new Size(228, 28);
            cboSheet.TabIndex = 1;
            // 
            // dgvFields
            // 
            dgvFields.AllowUserToAddRows = false;
            dgvFields.AllowUserToDeleteRows = false;
            dgvFields.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            dgvFields.Dock = DockStyle.Fill;
            dgvFields.Location = new Point(0, 0);
            dgvFields.Margin = new Padding(3, 4, 3, 4);
            dgvFields.Name = "dgvFields";
            dgvFields.RowHeadersWidth = 51;
            dgvFields.RowTemplate.Height = 25;
            dgvFields.Size = new Size(1330, 723);
            dgvFields.TabIndex = 2;
            // 
            // btnSettings
            // 
            btnSettings.Location = new Point(14, 9);
            btnSettings.Margin = new Padding(3, 4, 3, 4);
            btnSettings.Name = "btnSettings";
            btnSettings.Size = new Size(114, 35);
            btnSettings.TabIndex = 3;
            btnSettings.Text = "Settings";
            btnSettings.UseVisualStyleBackColor = true;
            // 
            // btnPreview
            // 
            btnPreview.Enabled = false;
            btnPreview.Location = new Point(143, 9);
            btnPreview.Margin = new Padding(3, 4, 3, 4);
            btnPreview.Name = "btnPreview";
            btnPreview.Size = new Size(114, 35);
            btnPreview.TabIndex = 4;
            btnPreview.Text = "Preview";
            btnPreview.UseVisualStyleBackColor = true;
            // 
            // btnExport
            // 
            btnExport.Enabled = false;
            btnExport.Location = new Point(269, 9);
            btnExport.Margin = new Padding(3, 4, 3, 4);
            btnExport.Name = "btnExport";
            btnExport.Size = new Size(114, 35);
            btnExport.TabIndex = 5;
            btnExport.Text = "Export .ctl";
            btnExport.UseVisualStyleBackColor = true;
            // 
            // btnDataPreview
            // 
            btnDataPreview.Enabled = false;
            btnDataPreview.Location = new Point(394, 9);
            btnDataPreview.Margin = new Padding(3, 4, 3, 4);
            btnDataPreview.Name = "btnDataPreview";
            btnDataPreview.Size = new Size(114, 35);
            btnDataPreview.TabIndex = 6;
            btnDataPreview.Text = "Data Preview";
            btnDataPreview.UseVisualStyleBackColor = true;
            // 
            // btnGenerateTable
            // 
            btnGenerateTable.Enabled = false;
            btnGenerateTable.Location = new Point(519, 9);
            btnGenerateTable.Margin = new Padding(3, 4, 3, 4);
            btnGenerateTable.Name = "btnGenerateTable";
            btnGenerateTable.Size = new Size(114, 35);
            btnGenerateTable.TabIndex = 7;
            btnGenerateTable.Text = "Generate Table";
            btnGenerateTable.UseVisualStyleBackColor = true;
            // 
            // lblStatus
            // 
            lblStatus.AutoSize = true;
            lblStatus.Location = new Point(14, 18);
            lblStatus.Name = "lblStatus";
            lblStatus.Size = new Size(50, 20);
            lblStatus.TabIndex = 6;
            lblStatus.Text = "Ready";
            // 
            // lblSheet
            // 
            lblSheet.AutoSize = true;
            lblSheet.Location = new Point(194, 21);
            lblSheet.Name = "lblSheet";
            lblSheet.Size = new Size(49, 20);
            lblSheet.TabIndex = 7;
            lblSheet.Text = "Sheet:";
            // 
            // panelTop
            // 
            panelTop.Controls.Add(lblSheet);
            panelTop.Controls.Add(cboSheet);
            panelTop.Controls.Add(btnLoadExcel);
            panelTop.Dock = DockStyle.Top;
            panelTop.Location = new Point(0, 0);
            panelTop.Margin = new Padding(3, 4, 3, 4);
            panelTop.Name = "panelTop";
            panelTop.Size = new Size(1330, 71);
            panelTop.TabIndex = 8;
            // 
            // panelBottom
            // 
            panelBottom.Controls.Add(panelStatus);
            panelBottom.Controls.Add(panelToolbar);
            panelBottom.Dock = DockStyle.Bottom;
            panelBottom.Location = new Point(0, 794);
            panelBottom.Margin = new Padding(3, 4, 3, 4);
            panelBottom.Name = "panelBottom";
            panelBottom.Size = new Size(1330, 141);
            panelBottom.TabIndex = 9;
            // 
            // panelStatus
            // 
            panelStatus.Controls.Add(lblStatus);
            panelStatus.Dock = DockStyle.Fill;
            panelStatus.Location = new Point(0, 100);
            panelStatus.Margin = new Padding(3, 4, 3, 4);
            panelStatus.Name = "panelStatus";
            panelStatus.Size = new Size(1330, 41);
            panelStatus.TabIndex = 19;
            // 
            // panelToolbar
            // 
            panelToolbar.Controls.Add(lblMode);
            panelToolbar.Controls.Add(btnAutoFix);
            panelToolbar.Controls.Add(btnValidate);
            panelToolbar.Controls.Add(btnToggleMode);
            panelToolbar.Controls.Add(btnRemoveField);
            panelToolbar.Controls.Add(btnAddField);
            panelToolbar.Controls.Add(btnStartFromScratch);
            panelToolbar.Controls.Add(btnSettings);
            panelToolbar.Controls.Add(btnPreview);
            panelToolbar.Controls.Add(btnExport);
            panelToolbar.Controls.Add(btnDataPreview);
            panelToolbar.Controls.Add(btnGenerateTable);
            panelToolbar.Controls.Add(btnDataPreview);
            panelToolbar.Dock = DockStyle.Top;
            panelToolbar.Location = new Point(0, 0);
            panelToolbar.Margin = new Padding(3, 4, 3, 4);
            panelToolbar.Name = "panelToolbar";
            panelToolbar.Size = new Size(1330, 100);
            panelToolbar.TabIndex = 18;
            // 
            // lblMode
            // 
            lblMode.AutoSize = true;
            lblMode.Font = new Font("Segoe UI", 9F, FontStyle.Bold);
            lblMode.Location = new Point(735, 68);
            lblMode.Name = "lblMode";
            lblMode.Size = new Size(93, 20);
            lblMode.TabIndex = 17;
            lblMode.Text = "Fixed Width";
            // 
            // btnAutoFix
            // 
            btnAutoFix.Location = new Point(611, 53);
            btnAutoFix.Margin = new Padding(3, 4, 3, 4);
            btnAutoFix.Name = "btnAutoFix";
            btnAutoFix.Size = new Size(91, 35);
            btnAutoFix.TabIndex = 16;
            btnAutoFix.Text = "Auto Fix";
            btnAutoFix.UseVisualStyleBackColor = true;
            // 
            // btnValidate
            // 
            btnValidate.Location = new Point(509, 53);
            btnValidate.Margin = new Padding(3, 4, 3, 4);
            btnValidate.Name = "btnValidate";
            btnValidate.Size = new Size(91, 35);
            btnValidate.TabIndex = 15;
            btnValidate.Text = "Validate";
            btnValidate.UseVisualStyleBackColor = true;
            // 
            // btnToggleMode
            // 
            btnToggleMode.Location = new Point(383, 53);
            btnToggleMode.Margin = new Padding(3, 4, 3, 4);
            btnToggleMode.Name = "btnToggleMode";
            btnToggleMode.Size = new Size(114, 35);
            btnToggleMode.TabIndex = 14;
            btnToggleMode.Text = "Toggle Mode";
            btnToggleMode.UseVisualStyleBackColor = true;
            // 
            // btnRemoveField
            // 
            btnRemoveField.Location = new Point(269, 53);
            btnRemoveField.Margin = new Padding(3, 4, 3, 4);
            btnRemoveField.Name = "btnRemoveField";
            btnRemoveField.Size = new Size(103, 35);
            btnRemoveField.TabIndex = 13;
            btnRemoveField.Text = "Remove Field";
            btnRemoveField.UseVisualStyleBackColor = true;
            // 
            // btnAddField
            // 
            btnAddField.Location = new Point(166, 53);
            btnAddField.Margin = new Padding(3, 4, 3, 4);
            btnAddField.Name = "btnAddField";
            btnAddField.Size = new Size(91, 35);
            btnAddField.TabIndex = 12;
            btnAddField.Text = "Add Field";
            btnAddField.UseVisualStyleBackColor = true;
            // 
            // btnStartFromScratch
            // 
            btnStartFromScratch.Location = new Point(14, 53);
            btnStartFromScratch.Margin = new Padding(3, 4, 3, 4);
            btnStartFromScratch.Name = "btnStartFromScratch";
            btnStartFromScratch.Size = new Size(137, 35);
            btnStartFromScratch.TabIndex = 11;
            btnStartFromScratch.Text = "Start from Scratch";
            btnStartFromScratch.UseVisualStyleBackColor = true;
            // 
            // panelMain
            // 
            panelMain.Controls.Add(dgvFields);
            panelMain.Dock = DockStyle.Fill;
            panelMain.Location = new Point(0, 71);
            panelMain.Margin = new Padding(3, 4, 3, 4);
            panelMain.Name = "panelMain";
            panelMain.Size = new Size(1330, 723);
            panelMain.TabIndex = 10;
            // 
            // MainForm
            // 
            AutoScaleDimensions = new SizeF(8F, 20F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(1330, 935);
            Controls.Add(panelMain);
            Controls.Add(panelBottom);
            Controls.Add(panelTop);
            Margin = new Padding(3, 4, 3, 4);
            MinimumSize = new Size(912, 698);
            Name = "MainForm";
            StartPosition = FormStartPosition.CenterScreen;
            Text = "Oracle SQL*Loader Control File Generator";
            ((System.ComponentModel.ISupportInitialize)dgvFields).EndInit();
            panelTop.ResumeLayout(false);
            panelTop.PerformLayout();
            panelBottom.ResumeLayout(false);
            panelStatus.ResumeLayout(false);
            panelStatus.PerformLayout();
            panelToolbar.ResumeLayout(false);
            panelToolbar.PerformLayout();
            panelMain.ResumeLayout(false);
            ResumeLayout(false);
        }

        #endregion

        private System.Windows.Forms.Button btnLoadExcel;
        private System.Windows.Forms.ComboBox cboSheet;
        private System.Windows.Forms.DataGridView dgvFields;
        private System.Windows.Forms.Button btnSettings;
        private System.Windows.Forms.Button btnPreview;
        private System.Windows.Forms.Button btnExport;
        private System.Windows.Forms.Button btnDataPreview;
        private System.Windows.Forms.Button btnGenerateTable;
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