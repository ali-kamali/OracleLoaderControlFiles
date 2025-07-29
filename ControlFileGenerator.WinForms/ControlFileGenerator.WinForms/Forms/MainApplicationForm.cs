using ControlFileGenerator.WinForms.Services;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class MainApplicationForm : Form
    {
        //private ApplicationDefinition _appDefinition;
        private readonly ConfigurationService _configService;
        //private readonly ScriptGenerator _scriptGenerator;

        public MainApplicationForm()
        {
            InitializeComponent();
            //_configService = new ConfigurationService();
            //_scriptGenerator = new ScriptGenerator();
            //_appDefinition = new ApplicationDefinition();
            LoadJobsList();
        }

        private void BtnBrowseDir_Click(object sender, EventArgs e)
        {
            using (var folderDialog = new FolderBrowserDialog())
            {
                folderDialog.Description = "Select Script Directory";
                if (folderDialog.ShowDialog() == DialogResult.OK)
                {
                    //txtScriptDirectory.Text = folderDialog.SelectedPath;
                }
            }
        }

        private void BtnAddJob_Click(object sender, EventArgs e)
        {
            //var jobForm = new JobConfigurationForm();
            //if (jobForm.ShowDialog() == DialogResult.OK)
            //{
            //    var job = jobForm.JobDefinition;
            //    job.JobId = GenerateJobId();
            //    job.CreatedDate = DateTime.Now;
            //    job.LastModified = DateTime.Now;
                
            //    _appDefinition.Jobs.Add(job);
            //    LoadJobsList();
            //}
        }

        private void BtnEditJob_Click(object sender, EventArgs e)
        {
            if (lstJobs.SelectedItems.Count == 0)
            {
                MessageBox.Show("Please select a job to edit.", "No Selection", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            var selectedJobId = lstJobs.SelectedItems[0].Text;
            //var job = _appDefinition.Jobs.FirstOrDefault(j => j.JobId == selectedJobId);
            
            //if (job != null)
            //{
            //    var jobForm = new JobConfigurationForm(job);
            //    if (jobForm.ShowDialog() == DialogResult.OK)
            //    {
            //        job.LastModified = DateTime.Now;
            //        LoadJobsList();
            //    }
            //}
        }

        private void BtnRemoveJob_Click(object sender, EventArgs e)
        {
            //if (lstJobs.SelectedItems.Count == 0)
            //{
            //    MessageBox.Show("Please select a job to remove.", "No Selection", MessageBoxButtons.OK, MessageBoxIcon.Information);
            //    return;
            //}

            //var selectedJobId = lstJobs.SelectedItems[0].Text;
            //var job = _appDefinition.Jobs.FirstOrDefault(j => j.JobId == selectedJobId);
            
            //if (job != null)
            //{
            //    var result = MessageBox.Show($"Are you sure you want to remove job '{job.JobName}'?", 
            //        "Confirm Removal", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                
            //    if (result == DialogResult.Yes)
            //    {
            //        _appDefinition.Jobs.Remove(job);
            //        LoadJobsList();
            //    }
            //}
        }

        private void BtnSave_Click(object sender, EventArgs e)
        {
            //try
            //{
            //    UpdateAppDefinitionFromForm();
                
            //    if (!_configService.ValidateApplicationDefinition(_appDefinition, out var errors))
            //    {
            //        MessageBox.Show($"Validation errors:\n{string.Join("\n", errors)}", 
            //            "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            //        return;
            //    }

            //    using (var saveDialog = new SaveFileDialog())
            //    {
            //        saveDialog.Filter = "JSON files (*.json)|*.json|All files (*.*)|*.*";
            //        saveDialog.Title = "Save Application Configuration";
            //        saveDialog.FileName = $"{_appDefinition.Name}_config.json";
                    
            //        if (saveDialog.ShowDialog() == DialogResult.OK)
            //        {
            //            _configService.SaveApplicationDefinition(_appDefinition, saveDialog.FileName);
            //            MessageBox.Show("Configuration saved successfully!", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            //        }
            //    }
            //}
            //catch (Exception ex)
            //{
            //    MessageBox.Show($"Error saving configuration: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            //}
        }

        private void BtnLoad_Click(object sender, EventArgs e)
        {
            //try
            //{
            //    using (var openDialog = new OpenFileDialog())
            //    {
            //        openDialog.Filter = "JSON files (*.json)|*.json|All files (*.*)|*.*";
            //        openDialog.Title = "Load Application Configuration";
                    
            //        if (openDialog.ShowDialog() == DialogResult.OK)
            //        {
            //            _appDefinition = _configService.LoadApplicationDefinition(openDialog.FileName);
            //            UpdateFormFromAppDefinition();
            //            LoadJobsList();
            //            MessageBox.Show("Configuration loaded successfully!", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            //        }
            //    }
            //}
            //catch (Exception ex)
            //{
            //    MessageBox.Show($"Error loading configuration: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            //}
        }

        private void BtnGenerate_Click(object sender, EventArgs e)
        {
            //try
            //{
            //    UpdateAppDefinitionFromForm();
                
            //    if (!_configService.ValidateApplicationDefinition(_appDefinition, out var errors))
            //    {
            //        MessageBox.Show($"Validation errors:\n{string.Join("\n", errors)}", 
            //            "Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            //        return;
            //    }

            //    _scriptGenerator.GenerateApplication(_appDefinition);
            //    MessageBox.Show($"Application generated successfully!\nScripts saved to: {_appDefinition.ScriptDirectory}", 
            //        "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
            //}
            //catch (Exception ex)
            //{
            //    MessageBox.Show($"Error generating application: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            //}
        }

        private void BtnExit_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void LoadJobsList()
        {
            //lstJobs.Items.Clear();
            //foreach (var job in _appDefinition.Jobs)
            //{
            //    var item = new ListViewItem(job.JobId);
            //    item.SubItems.Add(job.JobName);
            //    item.SubItems.Add(job.JobType.ToString());
            //    item.SubItems.Add(job.Description);
            //    item.SubItems.Add(job.CreatedDate.ToString("yyyy-MM-dd"));
            //    lstJobs.Items.Add(item);
            //}
        }

        //private void UpdateAppDefinitionFromForm()
        //{
        //    _appDefinition.Name = txtAppName.Text.Trim();
        //    _appDefinition.Description = txtDescription.Text.Trim();
        //    _appDefinition.ScriptDirectory = txtScriptDirectory.Text.Trim();
        //}

        //private void UpdateFormFromAppDefinition()
        //{
        //    txtAppName.Text = _appDefinition.Name;
        //    txtDescription.Text = _appDefinition.Description;
        //    txtScriptDirectory.Text = _appDefinition.ScriptDirectory;
        //}

        //private string GenerateJobId()
        //{
        //    var baseId = "JOB";
        //    var counter = 1;
        //    while (_appDefinition.Jobs.Any(j => j.JobId == $"{baseId}{counter:D3}"))
        //    {
        //        counter++;
        //    }
        //    return $"{baseId}{counter:D3}";
        //}
    }
} 