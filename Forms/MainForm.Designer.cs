namespace ControlFileGenerator.WinForms.Forms
{
    partial class MainForm
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
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.panel1 = new System.Windows.Forms.Panel();
            this.labelStatus = new System.Windows.Forms.Label();
            this.buttonValidateFields = new System.Windows.Forms.Button();
            this.buttonCalculatePositions = new System.Windows.Forms.Button();
            this.buttonExportControlFile = new System.Windows.Forms.Button();
            this.buttonPreviewControlFile = new System.Windows.Forms.Button();
            this.buttonOpenSettings = new System.Windows.Forms.Button();
            this.comboBoxSheets = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.buttonLoadExcel = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.dataGridViewFields = new System.Windows.Forms.DataGridView();
            this.tableLayoutPanel1.SuspendLayout();
            this.panel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewFields)).BeginInit();
            this.SuspendLayout();
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 1;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutPanel1.Controls.Add(this.panel1, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.dataGridViewFields, 0, 1);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 2;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 80F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(1200, 800);
            this.tableLayoutPanel1.TabIndex = 0;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.labelStatus);
            this.panel1.Controls.Add(this.buttonValidateFields);
            this.panel1.Controls.Add(this.buttonCalculatePositions);
            this.panel1.Controls.Add(this.buttonExportControlFile);
            this.panel1.Controls.Add(this.buttonPreviewControlFile);
            this.panel1.Controls.Add(this.buttonOpenSettings);
            this.panel1.Controls.Add(this.comboBoxSheets);
            this.panel1.Controls.Add(this.label2);
            this.panel1.Controls.Add(this.buttonLoadExcel);
            this.panel1.Controls.Add(this.label1);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(3, 3);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(1194, 74);
            this.panel1.TabIndex = 0;
            // 
            // labelStatus
            // 
            this.labelStatus.AutoSize = true;
            this.labelStatus.Location = new System.Drawing.Point(12, 50);
            this.labelStatus.Name = "labelStatus";
            this.labelStatus.Size = new System.Drawing.Size(39, 15);
            this.labelStatus.TabIndex = 9;
            this.labelStatus.Text = "Ready";
            // 
            // buttonValidateFields
            // 
            this.buttonValidateFields.Location = new System.Drawing.Point(1050, 15);
            this.buttonValidateFields.Name = "buttonValidateFields";
            this.buttonValidateFields.Size = new System.Drawing.Size(120, 30);
            this.buttonValidateFields.TabIndex = 8;
            this.buttonValidateFields.Text = "Validate Fields";
            this.buttonValidateFields.UseVisualStyleBackColor = true;
            // 
            // buttonCalculatePositions
            // 
            this.buttonCalculatePositions.Location = new System.Drawing.Point(920, 15);
            this.buttonCalculatePositions.Name = "buttonCalculatePositions";
            this.buttonCalculatePositions.Size = new System.Drawing.Size(120, 30);
            this.buttonCalculatePositions.TabIndex = 7;
            this.buttonCalculatePositions.Text = "Calculate Positions";
            this.buttonCalculatePositions.UseVisualStyleBackColor = true;
            // 
            // buttonExportControlFile
            // 
            this.buttonExportControlFile.Location = new System.Drawing.Point(790, 15);
            this.buttonExportControlFile.Name = "buttonExportControlFile";
            this.buttonExportControlFile.Size = new System.Drawing.Size(120, 30);
            this.buttonExportControlFile.TabIndex = 6;
            this.buttonExportControlFile.Text = "Export .ctl File";
            this.buttonExportControlFile.UseVisualStyleBackColor = true;
            // 
            // buttonPreviewControlFile
            // 
            this.buttonPreviewControlFile.Location = new System.Drawing.Point(660, 15);
            this.buttonPreviewControlFile.Name = "buttonPreviewControlFile";
            this.buttonPreviewControlFile.Size = new System.Drawing.Size(120, 30);
            this.buttonPreviewControlFile.TabIndex = 5;
            this.buttonPreviewControlFile.Text = "Preview Control File";
            this.buttonPreviewControlFile.UseVisualStyleBackColor = true;
            // 
            // buttonOpenSettings
            // 
            this.buttonOpenSettings.Location = new System.Drawing.Point(530, 15);
            this.buttonOpenSettings.Name = "buttonOpenSettings";
            this.buttonOpenSettings.Size = new System.Drawing.Size(120, 30);
            this.buttonOpenSettings.TabIndex = 4;
            this.buttonOpenSettings.Text = "Loader Settings";
            this.buttonOpenSettings.UseVisualStyleBackColor = true;
            // 
            // comboBoxSheets
            // 
            this.comboBoxSheets.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxSheets.FormattingEnabled = true;
            this.comboBoxSheets.Location = new System.Drawing.Point(280, 20);
            this.comboBoxSheets.Name = "comboBoxSheets";
            this.comboBoxSheets.Size = new System.Drawing.Size(150, 23);
            this.comboBoxSheets.TabIndex = 3;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(240, 23);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(34, 15);
            this.label2.TabIndex = 2;
            this.label2.Text = "Sheet:";
            // 
            // buttonLoadExcel
            // 
            this.buttonLoadExcel.Location = new System.Drawing.Point(120, 15);
            this.buttonLoadExcel.Name = "buttonLoadExcel";
            this.buttonLoadExcel.Size = new System.Drawing.Size(110, 30);
            this.buttonLoadExcel.TabIndex = 1;
            this.buttonLoadExcel.Text = "Load Excel";
            this.buttonLoadExcel.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Segoe UI", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.label1.Location = new System.Drawing.Point(12, 20);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(102, 21);
            this.label1.TabIndex = 0;
            this.label1.Text = "Field Mappings";
            // 
            // dataGridViewFields
            // 
            this.dataGridViewFields.AllowUserToAddRows = false;
            this.dataGridViewFields.AllowUserToDeleteRows = false;
            this.dataGridViewFields.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.dataGridViewFields.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridViewFields.Dock = System.Windows.Forms.DockStyle.Fill;
            this.dataGridViewFields.Location = new System.Drawing.Point(3, 83);
            this.dataGridViewFields.Name = "dataGridViewFields";
            this.dataGridViewFields.RowTemplate.Height = 25;
            this.dataGridViewFields.Size = new System.Drawing.Size(1194, 714);
            this.dataGridViewFields.TabIndex = 1;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1200, 800);
            this.Controls.Add(this.tableLayoutPanel1);
            this.Name = "MainForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Oracle SQL*Loader Control File Generator";
            this.tableLayoutPanel1.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewFields)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button buttonLoadExcel;
        private System.Windows.Forms.ComboBox comboBoxSheets;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button buttonOpenSettings;
        private System.Windows.Forms.Button buttonPreviewControlFile;
        private System.Windows.Forms.Button buttonExportControlFile;
        private System.Windows.Forms.Button buttonCalculatePositions;
        private System.Windows.Forms.Button buttonValidateFields;
        private System.Windows.Forms.Label labelStatus;
        private System.Windows.Forms.DataGridView dataGridViewFields;
    }
} 