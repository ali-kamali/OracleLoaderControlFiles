namespace ControlFileGenerator.WinForms.Forms
{
    partial class FieldEditorForm
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.textBoxDescription = new System.Windows.Forms.TextBox();
            this.label15 = new System.Windows.Forms.Label();
            this.checkBoxNullable = new System.Windows.Forms.CheckBox();
            this.comboBoxSqlType = new System.Windows.Forms.ComboBox();
            this.textBoxCobolType = new System.Windows.Forms.TextBox();
            this.label14 = new System.Windows.Forms.Label();
            this.label13 = new System.Windows.Forms.Label();
            this.textBoxFieldName = new System.Windows.Forms.TextBox();
            this.label12 = new System.Windows.Forms.Label();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.numericUpDownLength = new System.Windows.Forms.NumericUpDown();
            this.numericUpDownEndPosition = new System.Windows.Forms.NumericUpDown();
            this.numericUpDownStartPosition = new System.Windows.Forms.NumericUpDown();
            this.numericUpDownOrder = new System.Windows.Forms.NumericUpDown();
            this.label11 = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.tabPageAdvanced = new System.Windows.Forms.TabPage();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.textBoxDataFormat = new System.Windows.Forms.TextBox();
            this.textBoxDelimiter = new System.Windows.Forms.TextBox();
            this.textBoxEnclosedBy = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.buttonTestTransform = new System.Windows.Forms.Button();
            this.textBoxNullIfValue = new System.Windows.Forms.TextBox();
            this.textBoxDefaultValue = new System.Windows.Forms.TextBox();
            this.textBoxTransform = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.tabPagePreview = new System.Windows.Forms.TabPage();
            this.labelValidationStatus = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.textBoxPreview = new System.Windows.Forms.TextBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.buttonReset = new System.Windows.Forms.Button();
            this.buttonApply = new System.Windows.Forms.Button();
            this.buttonCancel = new System.Windows.Forms.Button();
            this.buttonOK = new System.Windows.Forms.Button();
            this.tabControl1.SuspendLayout();
            this.tabPageBasic.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownLength)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownEndPosition)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownStartPosition)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownOrder)).BeginInit();
            this.tabPageAdvanced.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.tabPagePreview.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPageBasic);
            this.tabControl1.Controls.Add(this.tabPageAdvanced);
            this.tabControl1.Controls.Add(this.tabPagePreview);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(600, 500);
            this.tabControl1.TabIndex = 0;
            // 
            // tabPageBasic
            // 
            this.tabPageBasic.Controls.Add(this.groupBox1);
            this.tabPageBasic.Controls.Add(this.groupBox2);
            this.tabPageBasic.Location = new System.Drawing.Point(4, 24);
            this.tabPageBasic.Name = "tabPageBasic";
            this.tabPageBasic.Padding = new System.Windows.Forms.Padding(3);
            this.tabPageBasic.Size = new System.Drawing.Size(592, 472);
            this.tabPageBasic.TabIndex = 0;
            this.tabPageBasic.Text = "Basic Properties";
            this.tabPageBasic.UseVisualStyleBackColor = true;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.textBoxDescription);
            this.groupBox1.Controls.Add(this.label15);
            this.groupBox1.Controls.Add(this.checkBoxNullable);
            this.groupBox1.Controls.Add(this.comboBoxSqlType);
            this.groupBox1.Controls.Add(this.textBoxCobolType);
            this.groupBox1.Controls.Add(this.label14);
            this.groupBox1.Controls.Add(this.label13);
            this.groupBox1.Controls.Add(this.textBoxFieldName);
            this.groupBox1.Controls.Add(this.label12);
            this.groupBox1.Location = new System.Drawing.Point(6, 6);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(580, 200);
            this.groupBox1.TabIndex = 1;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Field Information";
            // 
            // textBoxDescription
            // 
            this.textBoxDescription.Location = new System.Drawing.Point(120, 160);
            this.textBoxDescription.Name = "textBoxDescription";
            this.textBoxDescription.Size = new System.Drawing.Size(450, 23);
            this.textBoxDescription.TabIndex = 8;
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Location = new System.Drawing.Point(20, 163);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(70, 15);
            this.label15.TabIndex = 7;
            this.label15.Text = "Description:";
            // 
            // checkBoxNullable
            // 
            this.checkBoxNullable.AutoSize = true;
            this.checkBoxNullable.Location = new System.Drawing.Point(120, 130);
            this.checkBoxNullable.Name = "checkBoxNullable";
            this.checkBoxNullable.Size = new System.Drawing.Size(70, 19);
            this.checkBoxNullable.TabIndex = 6;
            this.checkBoxNullable.Text = "Nullable";
            this.checkBoxNullable.UseVisualStyleBackColor = true;
            // 
            // comboBoxSqlType
            // 
            this.comboBoxSqlType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxSqlType.FormattingEnabled = true;
            this.comboBoxSqlType.Items.AddRange(new object[] {
            "CHAR",
            "VARCHAR2",
            "NUMBER",
            "DATE",
            "TIMESTAMP",
            "CLOB",
            "BLOB"});
            this.comboBoxSqlType.Location = new System.Drawing.Point(120, 100);
            this.comboBoxSqlType.Name = "comboBoxSqlType";
            this.comboBoxSqlType.Size = new System.Drawing.Size(150, 23);
            this.comboBoxSqlType.TabIndex = 5;
            // 
            // textBoxCobolType
            // 
            this.textBoxCobolType.Location = new System.Drawing.Point(120, 70);
            this.textBoxCobolType.Name = "textBoxCobolType";
            this.textBoxCobolType.Size = new System.Drawing.Size(200, 23);
            this.textBoxCobolType.TabIndex = 4;
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(20, 133);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(55, 15);
            this.label14.TabIndex = 3;
            this.label14.Text = "Nullable:";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Location = new System.Drawing.Point(20, 103);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(60, 15);
            this.label13.TabIndex = 2;
            this.label13.Text = "SQL Type:";
            // 
            // textBoxFieldName
            // 
            this.textBoxFieldName.Location = new System.Drawing.Point(120, 40);
            this.textBoxFieldName.Name = "textBoxFieldName";
            this.textBoxFieldName.Size = new System.Drawing.Size(200, 23);
            this.textBoxFieldName.TabIndex = 1;
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(20, 43);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(75, 15);
            this.label12.TabIndex = 0;
            this.label12.Text = "Field Name:";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.numericUpDownLength);
            this.groupBox2.Controls.Add(this.numericUpDownEndPosition);
            this.groupBox2.Controls.Add(this.numericUpDownStartPosition);
            this.groupBox2.Controls.Add(this.numericUpDownOrder);
            this.groupBox2.Controls.Add(this.label11);
            this.groupBox2.Controls.Add(this.label10);
            this.groupBox2.Controls.Add(this.label9);
            this.groupBox2.Controls.Add(this.label8);
            this.groupBox2.Location = new System.Drawing.Point(6, 220);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(580, 150);
            this.groupBox2.TabIndex = 0;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Position Information";
            // 
            // numericUpDownLength
            // 
            this.numericUpDownLength.Location = new System.Drawing.Point(120, 110);
            this.numericUpDownLength.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.numericUpDownLength.Name = "numericUpDownLength";
            this.numericUpDownLength.Size = new System.Drawing.Size(100, 23);
            this.numericUpDownLength.TabIndex = 7;
            // 
            // numericUpDownEndPosition
            // 
            this.numericUpDownEndPosition.Location = new System.Drawing.Point(120, 80);
            this.numericUpDownEndPosition.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.numericUpDownEndPosition.Name = "numericUpDownEndPosition";
            this.numericUpDownEndPosition.Size = new System.Drawing.Size(100, 23);
            this.numericUpDownEndPosition.TabIndex = 6;
            // 
            // numericUpDownStartPosition
            // 
            this.numericUpDownStartPosition.Location = new System.Drawing.Point(120, 50);
            this.numericUpDownStartPosition.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.numericUpDownStartPosition.Name = "numericUpDownStartPosition";
            this.numericUpDownStartPosition.Size = new System.Drawing.Size(100, 23);
            this.numericUpDownStartPosition.TabIndex = 5;
            // 
            // numericUpDownOrder
            // 
            this.numericUpDownOrder.Location = new System.Drawing.Point(120, 20);
            this.numericUpDownOrder.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.numericUpDownOrder.Name = "numericUpDownOrder";
            this.numericUpDownOrder.Size = new System.Drawing.Size(100, 23);
            this.numericUpDownOrder.TabIndex = 4;
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(20, 113);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(50, 15);
            this.label11.TabIndex = 3;
            this.label11.Text = "Length:";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(20, 83);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(85, 15);
            this.label10.TabIndex = 2;
            this.label10.Text = "End Position:";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(20, 53);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(90, 15);
            this.label9.TabIndex = 1;
            this.label9.Text = "Start Position:";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(20, 23);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(40, 15);
            this.label8.TabIndex = 0;
            this.label8.Text = "Order:";
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
            this.groupBox4.Controls.Add(this.textBoxDataFormat);
            this.groupBox4.Controls.Add(this.textBoxDelimiter);
            this.groupBox4.Controls.Add(this.textBoxEnclosedBy);
            this.groupBox4.Controls.Add(this.label7);
            this.groupBox4.Controls.Add(this.label6);
            this.groupBox4.Controls.Add(this.label5);
            this.groupBox4.Location = new System.Drawing.Point(6, 200);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(580, 150);
            this.groupBox4.TabIndex = 1;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Formatting Options";
            // 
            // textBoxDataFormat
            // 
            this.textBoxDataFormat.Location = new System.Drawing.Point(120, 100);
            this.textBoxDataFormat.Name = "textBoxDataFormat";
            this.textBoxDataFormat.Size = new System.Drawing.Size(200, 23);
            this.textBoxDataFormat.TabIndex = 5;
            // 
            // textBoxDelimiter
            // 
            this.textBoxDelimiter.Location = new System.Drawing.Point(120, 70);
            this.textBoxDelimiter.Name = "textBoxDelimiter";
            this.textBoxDelimiter.Size = new System.Drawing.Size(100, 23);
            this.textBoxDelimiter.TabIndex = 4;
            // 
            // textBoxEnclosedBy
            // 
            this.textBoxEnclosedBy.Location = new System.Drawing.Point(120, 40);
            this.textBoxEnclosedBy.Name = "textBoxEnclosedBy";
            this.textBoxEnclosedBy.Size = new System.Drawing.Size(100, 23);
            this.textBoxEnclosedBy.TabIndex = 3;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(20, 103);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(75, 15);
            this.label7.TabIndex = 2;
            this.label7.Text = "Data Format:";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(20, 73);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(60, 15);
            this.label6.TabIndex = 1;
            this.label6.Text = "Delimiter:";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(20, 43);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(80, 15);
            this.label5.TabIndex = 0;
            this.label5.Text = "Enclosed By:";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.buttonTestTransform);
            this.groupBox3.Controls.Add(this.textBoxNullIfValue);
            this.groupBox3.Controls.Add(this.textBoxDefaultValue);
            this.groupBox3.Controls.Add(this.textBoxTransform);
            this.groupBox3.Controls.Add(this.label4);
            this.groupBox3.Controls.Add(this.label3);
            this.groupBox3.Controls.Add(this.label2);
            this.groupBox3.Location = new System.Drawing.Point(6, 6);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(580, 180);
            this.groupBox3.TabIndex = 0;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Transformations";
            // 
            // buttonTestTransform
            // 
            this.buttonTestTransform.Location = new System.Drawing.Point(480, 70);
            this.buttonTestTransform.Name = "buttonTestTransform";
            this.buttonTestTransform.Size = new System.Drawing.Size(90, 23);
            this.buttonTestTransform.TabIndex = 6;
            this.buttonTestTransform.Text = "Test Transform";
            this.buttonTestTransform.UseVisualStyleBackColor = true;
            // 
            // textBoxNullIfValue
            // 
            this.textBoxNullIfValue.Location = new System.Drawing.Point(120, 140);
            this.textBoxNullIfValue.Name = "textBoxNullIfValue";
            this.textBoxNullIfValue.Size = new System.Drawing.Size(200, 23);
            this.textBoxNullIfValue.TabIndex = 5;
            // 
            // textBoxDefaultValue
            // 
            this.textBoxDefaultValue.Location = new System.Drawing.Point(120, 110);
            this.textBoxDefaultValue.Name = "textBoxDefaultValue";
            this.textBoxDefaultValue.Size = new System.Drawing.Size(200, 23);
            this.textBoxDefaultValue.TabIndex = 4;
            // 
            // textBoxTransform
            // 
            this.textBoxTransform.Location = new System.Drawing.Point(120, 40);
            this.textBoxTransform.Multiline = true;
            this.textBoxTransform.Name = "textBoxTransform";
            this.textBoxTransform.Size = new System.Drawing.Size(350, 60);
            this.textBoxTransform.TabIndex = 3;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(20, 143);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(75, 15);
            this.label4.TabIndex = 2;
            this.label4.Text = "Null If Value:";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(20, 113);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(85, 15);
            this.label3.TabIndex = 1;
            this.label3.Text = "Default Value:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(20, 43);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(65, 15);
            this.label2.TabIndex = 0;
            this.label2.Text = "Transform:";
            // 
            // tabPagePreview
            // 
            this.tabPagePreview.Controls.Add(this.labelValidationStatus);
            this.tabPagePreview.Controls.Add(this.label1);
            this.tabPagePreview.Controls.Add(this.textBoxPreview);
            this.tabPagePreview.Location = new System.Drawing.Point(4, 24);
            this.tabPagePreview.Name = "tabPagePreview";
            this.tabPagePreview.Padding = new System.Windows.Forms.Padding(3);
            this.tabPagePreview.Size = new System.Drawing.Size(592, 472);
            this.tabPagePreview.TabIndex = 2;
            this.tabPagePreview.Text = "Preview";
            this.tabPagePreview.UseVisualStyleBackColor = true;
            // 
            // labelValidationStatus
            // 
            this.labelValidationStatus.AutoSize = true;
            this.labelValidationStatus.Location = new System.Drawing.Point(20, 50);
            this.labelValidationStatus.Name = "labelValidationStatus";
            this.labelValidationStatus.Size = new System.Drawing.Size(100, 15);
            this.labelValidationStatus.TabIndex = 2;
            this.labelValidationStatus.Text = "Validation Status";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(20, 20);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(120, 15);
            this.label1.TabIndex = 1;
            this.label1.Text = "Generated Control Line:";
            // 
            // textBoxPreview
            // 
            this.textBoxPreview.Font = new System.Drawing.Font("Consolas", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.textBoxPreview.Location = new System.Drawing.Point(20, 80);
            this.textBoxPreview.Multiline = true;
            this.textBoxPreview.Name = "textBoxPreview";
            this.textBoxPreview.ReadOnly = true;
            this.textBoxPreview.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.textBoxPreview.Size = new System.Drawing.Size(560, 380);
            this.textBoxPreview.TabIndex = 0;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.buttonReset);
            this.panel1.Controls.Add(this.buttonApply);
            this.panel1.Controls.Add(this.buttonCancel);
            this.panel1.Controls.Add(this.buttonOK);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(0, 500);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(600, 50);
            this.panel1.TabIndex = 1;
            // 
            // buttonReset
            // 
            this.buttonReset.Location = new System.Drawing.Point(420, 15);
            this.buttonReset.Name = "buttonReset";
            this.buttonReset.Size = new System.Drawing.Size(75, 23);
            this.buttonReset.TabIndex = 3;
            this.buttonReset.Text = "Reset";
            this.buttonReset.UseVisualStyleBackColor = true;
            // 
            // buttonApply
            // 
            this.buttonApply.Location = new System.Drawing.Point(330, 15);
            this.buttonApply.Name = "buttonApply";
            this.buttonApply.Size = new System.Drawing.Size(75, 23);
            this.buttonApply.TabIndex = 2;
            this.buttonApply.Text = "Apply";
            this.buttonApply.UseVisualStyleBackColor = true;
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
            // FieldEditorForm
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
            this.Name = "FieldEditorForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Field Editor";
            this.tabControl1.ResumeLayout(false);
            this.tabPageBasic.ResumeLayout(false);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownLength)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownEndPosition)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownStartPosition)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numericUpDownOrder)).EndInit();
            this.tabPageAdvanced.ResumeLayout(false);
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.tabPagePreview.ResumeLayout(false);
            this.tabPagePreview.PerformLayout();
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPageBasic;
        private System.Windows.Forms.TabPage tabPageAdvanced;
        private System.Windows.Forms.TabPage tabPagePreview;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.TextBox textBoxFieldName;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.TextBox textBoxCobolType;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.ComboBox comboBoxSqlType;
        private System.Windows.Forms.CheckBox checkBoxNullable;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.TextBox textBoxDescription;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.NumericUpDown numericUpDownOrder;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.NumericUpDown numericUpDownStartPosition;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.NumericUpDown numericUpDownEndPosition;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.NumericUpDown numericUpDownLength;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.TextBox textBoxTransform;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox textBoxDefaultValue;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox textBoxNullIfValue;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Button buttonTestTransform;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.TextBox textBoxEnclosedBy;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox textBoxDelimiter;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox textBoxDataFormat;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBoxPreview;
        private System.Windows.Forms.Label labelValidationStatus;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button buttonOK;
        private System.Windows.Forms.Button buttonCancel;
        private System.Windows.Forms.Button buttonApply;
        private System.Windows.Forms.Button buttonReset;
    }
} 