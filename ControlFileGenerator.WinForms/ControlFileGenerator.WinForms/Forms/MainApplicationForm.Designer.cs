namespace ControlFileGenerator.WinForms.Forms
{
    partial class MainApplicationForm
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
            grpAppDefinition = new GroupBox();
            lblScriptDir = new Label();
            txtScriptDirectory = new TextBox();
            btnBrowseDir = new Button();
            lblDescription = new Label();
            txtDescription = new TextBox();
            lblName = new Label();
            txtAppName = new TextBox();
            grpJobs = new GroupBox();
            btnRemoveJob = new Button();
            btnEditJob = new Button();
            btnAddJob = new Button();
            lstJobs = new ListView();
            columnJobId = new ColumnHeader();
            columnJobName = new ColumnHeader();
            columnJobType = new ColumnHeader();
            columnDescription = new ColumnHeader();
            columnCreated = new ColumnHeader();
            btnSave = new Button();
            btnLoad = new Button();
            btnGenerate = new Button();
            btnExit = new Button();
            grpAppDefinition.SuspendLayout();
            grpJobs.SuspendLayout();
            SuspendLayout();
            // 
            // grpAppDefinition
            // 
            grpAppDefinition.Controls.Add(lblScriptDir);
            grpAppDefinition.Controls.Add(txtScriptDirectory);
            grpAppDefinition.Controls.Add(btnBrowseDir);
            grpAppDefinition.Controls.Add(lblDescription);
            grpAppDefinition.Controls.Add(txtDescription);
            grpAppDefinition.Controls.Add(lblName);
            grpAppDefinition.Controls.Add(txtAppName);
            grpAppDefinition.Location = new Point(10, 10);
            grpAppDefinition.Name = "grpAppDefinition";
            grpAppDefinition.Size = new Size(760, 120);
            grpAppDefinition.TabIndex = 0;
            grpAppDefinition.TabStop = false;
            grpAppDefinition.Text = "Application Definition";
            // 
            // lblScriptDir
            // 
            lblScriptDir.AutoSize = true;
            lblScriptDir.Location = new Point(10, 85);
            lblScriptDir.Name = "lblScriptDir";
            lblScriptDir.Size = new Size(100, 20);
            lblScriptDir.TabIndex = 6;
            lblScriptDir.Text = "Script Directory:";
            // 
            // txtScriptDirectory
            // 
            txtScriptDirectory.Location = new Point(120, 82);
            txtScriptDirectory.Name = "txtScriptDirectory";
            txtScriptDirectory.Size = new Size(400, 27);
            txtScriptDirectory.TabIndex = 5;
            // 
            // btnBrowseDir
            // 
            btnBrowseDir.Location = new Point(530, 80);
            btnBrowseDir.Name = "btnBrowseDir";
            btnBrowseDir.Size = new Size(80, 30);
            btnBrowseDir.TabIndex = 4;
            btnBrowseDir.Text = "Browse";
            btnBrowseDir.UseVisualStyleBackColor = true;
            btnBrowseDir.Click += BtnBrowseDir_Click;
            // 
            // lblDescription
            // 
            lblDescription.AutoSize = true;
            lblDescription.Location = new Point(10, 55);
            lblDescription.Name = "lblDescription";
            lblDescription.Size = new Size(100, 20);
            lblDescription.TabIndex = 3;
            lblDescription.Text = "Description:";
            // 
            // txtDescription
            // 
            txtDescription.Location = new Point(120, 52);
            txtDescription.Name = "txtDescription";
            txtDescription.Size = new Size(400, 27);
            txtDescription.TabIndex = 2;
            // 
            // lblName
            // 
            lblName.AutoSize = true;
            lblName.Location = new Point(10, 25);
            lblName.Name = "lblName";
            lblName.Size = new Size(100, 20);
            lblName.TabIndex = 1;
            lblName.Text = "Application Name:";
            // 
            // txtAppName
            // 
            txtAppName.Location = new Point(120, 22);
            txtAppName.Name = "txtAppName";
            txtAppName.Size = new Size(200, 27);
            txtAppName.TabIndex = 0;
            // 
            // grpJobs
            // 
            grpJobs.Controls.Add(btnRemoveJob);
            grpJobs.Controls.Add(btnEditJob);
            grpJobs.Controls.Add(btnAddJob);
            grpJobs.Controls.Add(lstJobs);
            grpJobs.Location = new Point(10, 140);
            grpJobs.Name = "grpJobs";
            grpJobs.Size = new Size(760, 300);
            grpJobs.TabIndex = 1;
            grpJobs.TabStop = false;
            grpJobs.Text = "Jobs";
            // 
            // btnRemoveJob
            // 
            btnRemoveJob.Location = new Point(190, 230);
            btnRemoveJob.Name = "btnRemoveJob";
            btnRemoveJob.Size = new Size(80, 30);
            btnRemoveJob.TabIndex = 3;
            btnRemoveJob.Text = "Remove Job";
            btnRemoveJob.UseVisualStyleBackColor = true;
            btnRemoveJob.Click += BtnRemoveJob_Click;
            // 
            // btnEditJob
            // 
            btnEditJob.Location = new Point(100, 230);
            btnEditJob.Name = "btnEditJob";
            btnEditJob.Size = new Size(80, 30);
            btnEditJob.TabIndex = 2;
            btnEditJob.Text = "Edit Job";
            btnEditJob.UseVisualStyleBackColor = true;
            btnEditJob.Click += BtnEditJob_Click;
            // 
            // btnAddJob
            // 
            btnAddJob.Location = new Point(10, 230);
            btnAddJob.Name = "btnAddJob";
            btnAddJob.Size = new Size(80, 30);
            btnAddJob.TabIndex = 1;
            btnAddJob.Text = "Add Job";
            btnAddJob.UseVisualStyleBackColor = true;
            btnAddJob.Click += BtnAddJob_Click;
            // 
            // lstJobs
            // 
            lstJobs.Columns.AddRange(new ColumnHeader[] { columnJobId, columnJobName, columnJobType, columnDescription, columnCreated });
            lstJobs.FullRowSelect = true;
            lstJobs.GridLines = true;
            lstJobs.Location = new Point(10, 20);
            lstJobs.Name = "lstJobs";
            lstJobs.Size = new Size(740, 200);
            lstJobs.TabIndex = 0;
            lstJobs.UseCompatibleStateImageBehavior = false;
            lstJobs.View = View.Details;
            // 
            // columnJobId
            // 
            columnJobId.Text = "Job ID";
            columnJobId.Width = 100;
            // 
            // columnJobName
            // 
            columnJobName.Text = "Job Name";
            columnJobName.Width = 150;
            // 
            // columnJobType
            // 
            columnJobType.Text = "Type";
            columnJobType.Width = 80;
            // 
            // columnDescription
            // 
            columnDescription.Text = "Description";
            columnDescription.Width = 200;
            // 
            // columnCreated
            // 
            columnCreated.Text = "Created";
            columnCreated.Width = 120;
            // 
            // btnSave
            // 
            btnSave.Location = new Point(10, 450);
            btnSave.Name = "btnSave";
            btnSave.Size = new Size(120, 35);
            btnSave.TabIndex = 2;
            btnSave.Text = "Save Configuration";
            btnSave.UseVisualStyleBackColor = true;
            btnSave.Click += BtnSave_Click;
            // 
            // btnLoad
            // 
            btnLoad.Location = new Point(140, 450);
            btnLoad.Name = "btnLoad";
            btnLoad.Size = new Size(120, 35);
            btnLoad.TabIndex = 3;
            btnLoad.Text = "Load Configuration";
            btnLoad.UseVisualStyleBackColor = true;
            btnLoad.Click += BtnLoad_Click;
            // 
            // btnGenerate
            // 
            btnGenerate.Location = new Point(270, 450);
            btnGenerate.Name = "btnGenerate";
            btnGenerate.Size = new Size(140, 35);
            btnGenerate.TabIndex = 4;
            btnGenerate.Text = "Generate Application";
            btnGenerate.UseVisualStyleBackColor = true;
            btnGenerate.Click += BtnGenerate_Click;
            // 
            // btnExit
            // 
            btnExit.Location = new Point(670, 450);
            btnExit.Name = "btnExit";
            btnExit.Size = new Size(80, 35);
            btnExit.TabIndex = 5;
            btnExit.Text = "Exit";
            btnExit.UseVisualStyleBackColor = true;
            btnExit.Click += BtnExit_Click;
            // 
            // MainForm
            // 
            AutoScaleDimensions = new SizeF(8F, 20F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(800, 500);
            Controls.Add(btnExit);
            Controls.Add(btnGenerate);
            Controls.Add(btnLoad);
            Controls.Add(btnSave);
            Controls.Add(grpJobs);
            Controls.Add(grpAppDefinition);
            FormBorderStyle = FormBorderStyle.FixedSingle;
            MaximizeBox = false;
            Name = "MainForm";
            StartPosition = FormStartPosition.CenterScreen;
            Text = "KSH Application Generator";
            grpAppDefinition.ResumeLayout(false);
            grpAppDefinition.PerformLayout();
            grpJobs.ResumeLayout(false);
            ResumeLayout(false);
        }

        #endregion

        private GroupBox grpAppDefinition;
        private Label lblScriptDir;
        private TextBox txtScriptDirectory;
        private Button btnBrowseDir;
        private Label lblDescription;
        private TextBox txtDescription;
        private Label lblName;
        private TextBox txtAppName;
        private GroupBox grpJobs;
        private Button btnRemoveJob;
        private Button btnEditJob;
        private Button btnAddJob;
        private ListView lstJobs;
        private ColumnHeader columnJobId;
        private ColumnHeader columnJobName;
        private ColumnHeader columnJobType;
        private ColumnHeader columnDescription;
        private ColumnHeader columnCreated;
        private Button btnSave;
        private Button btnLoad;
        private Button btnGenerate;
        private Button btnExit;
    }
} 