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
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPageBasic = new System.Windows.Forms.TabPage();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.textBoxEncoding = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.checkBoxTrailingNullCols = new System.Windows.Forms.CheckBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.buttonBrowseDiscardfile = new System.Windows.Forms.Button();
            this.buttonBrowseBadfile = new System.Windows.Forms.Button();
            this.buttonBrowseInfile = new System.Windows.Forms.Button();
            this.textBoxDiscardfile = new System.Windows.Forms.TextBox();
            this.textBoxBadfile = new System.Windows.Forms.TextBox();
            this.textBoxInfile = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.comboBoxLoadMode = new System.Windows.Forms.ComboBox();
            this.textBoxTableName = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.tabPageAdvanced = new System.Windows.Forms.TabPage();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.checkBoxPreserveBlanks = new System.Windows.Forms.CheckBox();
            this.comboBoxTrimOption = new System.Windows.Forms.ComboBox();
            this.label15 = new System.Windows.Forms.Label();
            this.checkBoxOptionallyEnclosed = new System.Windows.Forms.CheckBox();
            this.textBoxEnclosedBy = new System.Windows.Forms.TextBox();
            this.textBoxFieldTerminator = new System.Windows.Forms.TextBox();
            this.textBoxCharacterSet = new System.Windows.Forms.TextBox();
            this.label14 = new System.Windows.Forms.Label();
            this.label13 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.numericUpDownLoadRows = new System.Windows.Forms.NumericUpDown();
            this.numericUpDownSkipRows = new System.Windows.Forms.NumericUpDown();
            this.numericUpDownRows = new System.Windows.Forms.NumericUpDown();
            this.numericUpDownBindSize = new System.Windows.Forms.NumericUpDown();
            this.numericUpDownErrors = new System.Windows.Forms.NumericUpDown();
            this.checkBoxDirectPath = new System.Windows.Forms.CheckBox();
            this.label10 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.label16 = new System.Windows.Forms.Label();
            this.panel1 = new System.Windows.Forms.Panel();
            this.buttonCancel = new System.Windows.Forms.Button();
            this.buttonOK = new System.Windows.Forms.Button();
            this.tabControl1.SuspendLayout();
            this.tabPageBasic.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.tabPageAdvanced.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.groupBox3.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownLoadRows)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownSkipRows)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownRows)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownBindSize)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownErrors)).BeginInit();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPageBasic);
            this.tabControl1.Controls.Add(this.tabPageAdvanced);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(600, 500);
            this.tabControl1.TabIndex = 0;
            // 
            // tabPageBasic
            // 
            this.tabPageBasic.Controls.Add(this.groupBox2);
            this.tabPageBasic.Controls.Add(this.groupBox1);
            this.tabPageBasic.Location = new System.Drawing.Point(4, 24);
            this.tabPageBasic.Name = "tabPageBasic";
            this.tabPageBasic.Padding = new System.Windows.Forms.Padding(3);
            this.tabPageBasic.Size = new System.Drawing.Size(592, 472);
            this.tabPageBasic.TabIndex = 0;
            this.tabPageBasic.Text = "Basic Settings";
            this.tabPageBasic.UseVisualStyleBackColor = true;
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.textBoxEncoding);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.checkBoxTrailingNullCols);
            this.groupBox2.Controls.Add(this.comboBoxLoadMode);
            this.groupBox2.Controls.Add(this.textBoxTableName);
            this.groupBox2.Controls.Add(this.label2);
            this.groupBox2.Controls.Add(this.label1);
            this.groupBox2.Location = new System.Drawing.Point(6, 6);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(580, 150);
            this.groupBox2.TabIndex = 1;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Oracle Table Settings";
            // 
            // textBoxEncoding
            // 
            this.textBoxEncoding.Location = new System.Drawing.Point(120, 110);
            this.textBoxEncoding.Name = "textBoxEncoding";
            this.textBoxEncoding.Size = new System.Drawing.Size(150, 23);
            this.textBoxEncoding.TabIndex = 6;
            this.textBoxEncoding.Text = "UTF8";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(20, 113);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(58, 15);
            this.label6.TabIndex = 5;
            this.label6.Text = "Encoding:";
            // 
            // checkBoxTrailingNullCols
            // 
            this.checkBoxTrailingNullCols.AutoSize = true;
            this.checkBoxTrailingNullCols.Location = new System.Drawing.Point(300, 80);
            this.checkBoxTrailingNullCols.Name = "checkBoxTrailingNullCols";
            this.checkBoxTrailingNullCols.Size = new System.Drawing.Size(120, 19);
            this.checkBoxTrailingNullCols.TabIndex = 4;
            this.checkBoxTrailingNullCols.Text = "TRAILING NULLCOLS";
            this.checkBoxTrailingNullCols.UseVisualStyleBackColor = true;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.buttonBrowseDiscardfile);
            this.groupBox1.Controls.Add(this.buttonBrowseBadfile);
            this.groupBox1.Controls.Add(this.buttonBrowseInfile);
            this.groupBox1.Controls.Add(this.textBoxDiscardfile);
            this.groupBox1.Controls.Add(this.textBoxBadfile);
            this.groupBox1.Controls.Add(this.textBoxInfile);
            this.groupBox1.Controls.Add(this.label5);
            this.groupBox1.Controls.Add(this.label4);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Location = new System.Drawing.Point(6, 170);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(580, 150);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "File References";
            // 
            // buttonBrowseDiscardfile
            // 
            this.buttonBrowseDiscardfile.Location = new System.Drawing.Point(480, 110);
            this.buttonBrowseDiscardfile.Name = "buttonBrowseDiscardfile";
            this.buttonBrowseDiscardfile.Size = new System.Drawing.Size(75, 23);
            this.buttonBrowseDiscardfile.TabIndex = 8;
            this.buttonBrowseDiscardfile.Text = "Browse";
            this.buttonBrowseDiscardfile.UseVisualStyleBackColor = true;
            // 
            // buttonBrowseBadfile
            // 
            this.buttonBrowseBadfile.Location = new System.Drawing.Point(480, 80);
            this.buttonBrowseBadfile.Name = "buttonBrowseBadfile";
            this.buttonBrowseBadfile.Size = new System.Drawing.Size(75, 23);
            this.buttonBrowseBadfile.TabIndex = 7;
            this.buttonBrowseBadfile.Text = "Browse";
            this.buttonBrowseBadfile.UseVisualStyleBackColor = true;
            // 
            // buttonBrowseInfile
            // 
            this.buttonBrowseInfile.Location = new System.Drawing.Point(480, 50);
            this.buttonBrowseInfile.Name = "buttonBrowseInfile";
            this.buttonBrowseInfile.Size = new System.Drawing.Size(75, 23);
            this.buttonBrowseInfile.TabIndex = 6;
            this.buttonBrowseInfile.Text = "Browse";
            this.buttonBrowseInfile.UseVisualStyleBackColor = true;
            // 
            // textBoxDiscardfile
            // 
            this.textBoxDiscardfile.Location = new System.Drawing.Point(120, 110);
            this.textBoxDiscardfile.Name = "textBoxDiscardfile";
            this.textBoxDiscardfile.Size = new System.Drawing.Size(350, 23);
            this.textBoxDiscardfile.TabIndex = 5;
            // 
            // textBoxBadfile
            // 
            this.textBoxBadfile.Location = new System.Drawing.Point(120, 80);
            this.textBoxBadfile.Name = "textBoxBadfile";
            this.textBoxBadfile.Size = new System.Drawing.Size(350, 23);
            this.textBoxBadfile.TabIndex = 4;
            // 
            // textBoxInfile
            // 
            this.textBoxInfile.Location = new System.Drawing.Point(120, 50);
            this.textBoxInfile.Name = "textBoxInfile";
            this.textBoxInfile.Size = new System.Drawing.Size(350, 23);
            this.textBoxInfile.TabIndex = 3;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(20, 113);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(70, 15);
            this.label5.TabIndex = 2;
            this.label5.Text = "Discard File:";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(20, 83);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(55, 15);
            this.label4.TabIndex = 1;
            this.label4.Text = "Bad File:";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(20, 53);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(60, 15);
            this.label3.TabIndex = 0;
            this.label3.Text = "Input File:";
            // 
            // comboBoxLoadMode
            // 
            this.comboBoxLoadMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxLoadMode.FormattingEnabled = true;
            this.comboBoxLoadMode.Items.AddRange(new object[] {
            "APPEND",
            "REPLACE",
            "INSERT",
            "TRUNCATE"});
            this.comboBoxLoadMode.Location = new System.Drawing.Point(120, 80);
            this.comboBoxLoadMode.Name = "comboBoxLoadMode";
            this.comboBoxLoadMode.Size = new System.Drawing.Size(150, 23);
            this.comboBoxLoadMode.TabIndex = 3;
            // 
            // textBoxTableName
            // 
            this.textBoxTableName.Location = new System.Drawing.Point(120, 50);
            this.textBoxTableName.Name = "textBoxTableName";
            this.textBoxTableName.Size = new System.Drawing.Size(200, 23);
            this.textBoxTableName.TabIndex = 2;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(20, 83);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(75, 15);
            this.label2.TabIndex = 1;
            this.label2.Text = "Load Mode:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(20, 53);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(80, 15);
            this.label1.TabIndex = 0;
            this.label1.Text = "Table Name:";
            // 
            // tabPageAdvanced
            // 
            this.tabPageAdvanced.Controls.Add(this.groupBox4);
            this.tabPageAdvanced.Controls.Add(this.groupBox3);
            this.tabPageAdvanced.Location = new System.Drawing.Point(4, 24);
            this.tabPageAdvanced.Name = "tabPageAdvanced";
            this.tabPageAdvanced.Padding = new System.Windows.Forms.Padding(3);
            this.tabPageAdvanced.Size = new System.Drawing.Size(592, 472);
            this.tabPageAdvanced.TabIndex = 1;
            this.tabPageAdvanced.Text = "Advanced Options";
            this.tabPageAdvanced.UseVisualStyleBackColor = true;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.checkBoxPreserveBlanks);
            this.groupBox4.Controls.Add(this.comboBoxTrimOption);
            this.groupBox4.Controls.Add(this.label15);
            this.groupBox4.Controls.Add(this.checkBoxOptionallyEnclosed);
            this.groupBox4.Controls.Add(this.textBoxEnclosedBy);
            this.groupBox4.Controls.Add(this.textBoxFieldTerminator);
            this.groupBox4.Controls.Add(this.textBoxCharacterSet);
            this.groupBox4.Controls.Add(this.label14);
            this.groupBox4.Controls.Add(this.label13);
            this.groupBox4.Controls.Add(this.label12);
            this.groupBox4.Controls.Add(this.label11);
            this.groupBox4.Location = new System.Drawing.Point(6, 200);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(580, 200);
            this.groupBox4.TabIndex = 1;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Field Specifications";
            // 
            // checkBoxPreserveBlanks
            // 
            this.checkBoxPreserveBlanks.AutoSize = true;
            this.checkBoxPreserveBlanks.Location = new System.Drawing.Point(300, 160);
            this.checkBoxPreserveBlanks.Name = "checkBoxPreserveBlanks";
            this.checkBoxPreserveBlanks.Size = new System.Drawing.Size(110, 19);
            this.checkBoxPreserveBlanks.TabIndex = 10;
            this.checkBoxPreserveBlanks.Text = "Preserve Blanks";
            this.checkBoxPreserveBlanks.UseVisualStyleBackColor = true;
            // 
            // comboBoxTrimOption
            // 
            this.comboBoxTrimOption.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxTrimOption.FormattingEnabled = true;
            this.comboBoxTrimOption.Items.AddRange(new object[] {
            "LRTRIM",
            "LTRIM",
            "RTRIM",
            "NOTRIM"});
            this.comboBoxTrimOption.Location = new System.Drawing.Point(120, 160);
            this.comboBoxTrimOption.Name = "comboBoxTrimOption";
            this.comboBoxTrimOption.Size = new System.Drawing.Size(150, 23);
            this.comboBoxTrimOption.TabIndex = 9;
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Location = new System.Drawing.Point(20, 163);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(80, 15);
            this.label15.TabIndex = 8;
            this.label15.Text = "Trim Option:";
            // 
            // checkBoxOptionallyEnclosed
            // 
            this.checkBoxOptionallyEnclosed.AutoSize = true;
            this.checkBoxOptionallyEnclosed.Location = new System.Drawing.Point(300, 130);
            this.checkBoxOptionallyEnclosed.Name = "checkBoxOptionallyEnclosed";
            this.checkBoxOptionallyEnclosed.Size = new System.Drawing.Size(140, 19);
            this.checkBoxOptionallyEnclosed.TabIndex = 7;
            this.checkBoxOptionallyEnclosed.Text = "Optionally Enclosed";
            this.checkBoxOptionallyEnclosed.UseVisualStyleBackColor = true;
            // 
            // textBoxEnclosedBy
            // 
            this.textBoxEnclosedBy.Location = new System.Drawing.Point(120, 130);
            this.textBoxEnclosedBy.Name = "textBoxEnclosedBy";
            this.textBoxEnclosedBy.Size = new System.Drawing.Size(150, 23);
            this.textBoxEnclosedBy.TabIndex = 6;
            this.textBoxEnclosedBy.Text = "\"";
            // 
            // textBoxFieldTerminator
            // 
            this.textBoxFieldTerminator.Location = new System.Drawing.Point(120, 100);
            this.textBoxFieldTerminator.Name = "textBoxFieldTerminator";
            this.textBoxFieldTerminator.Size = new System.Drawing.Size(150, 23);
            this.textBoxFieldTerminator.TabIndex = 5;
            this.textBoxFieldTerminator.Text = ",";
            // 
            // textBoxCharacterSet
            // 
            this.textBoxCharacterSet.Location = new System.Drawing.Point(120, 70);
            this.textBoxCharacterSet.Name = "textBoxCharacterSet";
            this.textBoxCharacterSet.Size = new System.Drawing.Size(150, 23);
            this.textBoxCharacterSet.TabIndex = 4;
            this.textBoxCharacterSet.Text = "UTF8";
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(20, 133);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(80, 15);
            this.label14.TabIndex = 3;
            this.label14.Text = "Enclosed By:";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(20, 103);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(95, 15);
            this.label13.TabIndex = 2;
            this.label13.Text = "Field Terminator:";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(20, 73);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(85, 15);
            this.label12.TabIndex = 1;
            this.label12.Text = "Character Set:";
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(20, 43);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(100, 15);
            this.label11.TabIndex = 0;
            this.label11.Text = "Processing Options";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.numericUpDownLoadRows);
            this.groupBox3.Controls.Add(this.numericUpDownSkipRows);
            this.groupBox3.Controls.Add(this.numericUpDownRows);
            this.groupBox3.Controls.Add(this.numericUpDownBindSize);
            this.groupBox3.Controls.Add(this.numericUpDownErrors);
            this.groupBox3.Controls.Add(this.checkBoxDirectPath);
            this.groupBox3.Controls.Add(this.label10);
            this.groupBox3.Controls.Add(this.label9);
            this.groupBox3.Controls.Add(this.label8);
            this.groupBox3.Controls.Add(this.label7);
            this.groupBox3.Controls.Add(this.label16);
            this.groupBox3.Location = new System.Drawing.Point(6, 6);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(580, 180);
            this.groupBox3.TabIndex = 0;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Processing Options";
            // 
            // numericUpDownLoadRows
            // 
            this.numericUpDownLoadRows.Location = new System.Drawing.Point(120, 140);
            this.numericUpDownLoadRows.Maximum = new decimal(new int[] {
            999999999,
            0,
            0,
            0});
            this.numericUpDownLoadRows.Name = "numericUpDownLoadRows";
            this.numericUpDownLoadRows.Size = new System.Drawing.Size(150, 23);
            this.numericUpDownLoadRows.TabIndex = 10;
            // 
            // numericUpDownSkipRows
            // 
            this.numericUpDownSkipRows.Location = new System.Drawing.Point(120, 110);
            this.numericUpDownSkipRows.Maximum = new decimal(new int[] {
            999999999,
            0,
            0,
            0});
            this.numericUpDownSkipRows.Name = "numericUpDownSkipRows";
            this.numericUpDownSkipRows.Size = new System.Drawing.Size(150, 23);
            this.numericUpDownSkipRows.TabIndex = 9;
            // 
            // numericUpDownRows
            // 
            this.numericUpDownRows.Location = new System.Drawing.Point(120, 80);
            this.numericUpDownRows.Maximum = new decimal(new int[] {
            999999999,
            0,
            0,
            0});
            this.numericUpDownRows.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numericUpDownRows.Name = "numericUpDownRows";
            this.numericUpDownRows.Size = new System.Drawing.Size(150, 23);
            this.numericUpDownRows.TabIndex = 8;
            this.numericUpDownRows.Value = new decimal(new int[] {
            50000,
            0,
            0,
            0});
            // 
            // numericUpDownBindSize
            // 
            this.numericUpDownBindSize.Increment = new decimal(new int[] {
            1024,
            0,
            0,
            0});
            this.numericUpDownBindSize.Location = new System.Drawing.Point(120, 50);
            this.numericUpDownBindSize.Maximum = new decimal(new int[] {
            999999999,
            0,
            0,
            0});
            this.numericUpDownBindSize.Minimum = new decimal(new int[] {
            1024,
            0,
            0,
            0});
            this.numericUpDownBindSize.Name = "numericUpDownBindSize";
            this.numericUpDownBindSize.Size = new System.Drawing.Size(150, 23);
            this.numericUpDownBindSize.TabIndex = 7;
            this.numericUpDownBindSize.Value = new decimal(new int[] {
            1048576,
            0,
            0,
            0});
            // 
            // numericUpDownErrors
            // 
            this.numericUpDownErrors.Location = new System.Drawing.Point(120, 20);
            this.numericUpDownErrors.Maximum = new decimal(new int[] {
            999999999,
            0,
            0,
            0});
            this.numericUpDownErrors.Name = "numericUpDownErrors";
            this.numericUpDownErrors.Size = new System.Drawing.Size(150, 23);
            this.numericUpDownErrors.TabIndex = 6;
            this.numericUpDownErrors.Value = new decimal(new int[] {
            50,
            0,
            0,
            0});
            // 
            // checkBoxDirectPath
            // 
            this.checkBoxDirectPath.AutoSize = true;
            this.checkBoxDirectPath.Location = new System.Drawing.Point(300, 20);
            this.checkBoxDirectPath.Name = "checkBoxDirectPath";
            this.checkBoxDirectPath.Size = new System.Drawing.Size(90, 19);
            this.checkBoxDirectPath.TabIndex = 5;
            this.checkBoxDirectPath.Text = "DIRECT=TRUE";
            this.checkBoxDirectPath.UseVisualStyleBackColor = true;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(20, 143);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(75, 15);
            this.label10.TabIndex = 4;
            this.label10.Text = "Load Rows:";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(20, 113);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(70, 15);
            this.label9.TabIndex = 3;
            this.label9.Text = "Skip Rows:";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(20, 83);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(40, 15);
            this.label8.TabIndex = 2;
            this.label8.Text = "Rows:";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(20, 53);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(65, 15);
            this.label7.TabIndex = 1;
            this.label7.Text = "Bind Size:";
            // 
            // label16
            // 
            this.label16.AutoSize = true;
            this.label16.Location = new System.Drawing.Point(20, 23);
            this.label16.Name = "label16";
            this.label16.Size = new System.Drawing.Size(75, 15);
            this.label16.TabIndex = 0;
            this.label16.Text = "Max Errors:";
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.buttonCancel);
            this.panel1.Controls.Add(this.buttonOK);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(0, 500);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(600, 50);
            this.panel1.TabIndex = 1;
            // 
            // buttonCancel
            // 
            this.buttonCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.buttonCancel.Location = new System.Drawing.Point(510, 15);
            this.buttonCancel.Name = "buttonCancel";
            this.buttonCancel.Size = new System.Drawing.Size(75, 23);
            this.buttonCancel.TabIndex = 1;
            this.buttonCancel.Text = "Cancel";
            this.buttonCancel.UseVisualStyleBackColor = true;
            // 
            // buttonOK
            // 
            this.buttonOK.Location = new System.Drawing.Point(420, 15);
            this.buttonOK.Name = "buttonOK";
            this.buttonOK.Size = new System.Drawing.Size(75, 23);
            this.buttonOK.TabIndex = 0;
            this.buttonOK.Text = "OK";
            this.buttonOK.UseVisualStyleBackColor = true;
            // 
            // SettingsForm
            // 
            this.AcceptButton = this.buttonOK;
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.buttonCancel;
            this.ClientSize = new System.Drawing.Size(600, 550);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.panel1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "SettingsForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "SQL*Loader Settings";
            this.tabControl1.ResumeLayout(false);
            this.tabPageBasic.ResumeLayout(false);
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.tabPageAdvanced.ResumeLayout(false);
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownLoadRows)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownSkipRows)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownRows)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownBindSize)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownErrors)).EndInit();
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPageBasic;
        private System.Windows.Forms.TabPage tabPageAdvanced;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox textBoxInfile;
        private System.Windows.Forms.TextBox textBoxBadfile;
        private System.Windows.Forms.TextBox textBoxDiscardfile;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Button buttonBrowseInfile;
        private System.Windows.Forms.Button buttonBrowseBadfile;
        private System.Windows.Forms.Button buttonBrowseDiscardfile;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBoxTableName;
        private System.Windows.Forms.ComboBox comboBoxLoadMode;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.CheckBox checkBoxTrailingNullCols;
        private System.Windows.Forms.TextBox textBoxEncoding;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.Label label16;
        private System.Windows.Forms.CheckBox checkBoxDirectPath;
        private System.Windows.Forms.NumericUpDown numericUpDownErrors;
        private System.Windows.Forms.NumericUpDown numericUpDownBindSize;
        private System.Windows.Forms.NumericUpDown numericUpDownRows;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.NumericUpDown numericUpDownSkipRows;
        private System.Windows.Forms.NumericUpDown numericUpDownLoadRows;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.TextBox textBoxCharacterSet;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.TextBox textBoxFieldTerminator;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.TextBox textBoxEnclosedBy;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.CheckBox checkBoxOptionallyEnclosed;
        private System.Windows.Forms.ComboBox comboBoxTrimOption;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.CheckBox checkBoxPreserveBlanks;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button buttonOK;
        private System.Windows.Forms.Button buttonCancel;
    }
} 