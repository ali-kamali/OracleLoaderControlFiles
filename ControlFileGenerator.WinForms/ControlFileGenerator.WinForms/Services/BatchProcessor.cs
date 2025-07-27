using ControlFileGenerator.WinForms.Models;
using System.Text;

namespace ControlFileGenerator.WinForms.Services
{
    public class BatchProcessor
    {
        private readonly Services.ControlFileGenerator _controlFileGenerator;
        private readonly LoggingService _loggingService;
        private readonly TemplateManager _templateManager;

        public BatchProcessor()
        {
            _controlFileGenerator = new Services.ControlFileGenerator();
            _loggingService = new LoggingService();
            _templateManager = new TemplateManager();
        }

        /// <summary>
        /// Processes multiple files using a template
        /// </summary>
        public async Task<BatchResult> ProcessBatchAsync(BatchConfiguration config)
        {
            var result = new BatchResult
            {
                StartTime = DateTime.Now,
                TotalFiles = config.InputFiles.Count
            };

            try
            {
                _loggingService.LogInformation($"Starting batch processing for {config.InputFiles.Count} files");

                foreach (var inputFile in config.InputFiles)
                {
                    try
                    {
                        var fileResult = await ProcessSingleFileAsync(inputFile, config);
                        result.ProcessedFiles.Add(fileResult);

                        if (fileResult.Success)
                        {
                            result.SuccessCount++;
                        }
                        else
                        {
                            result.ErrorCount++;
                        }
                    }
                    catch (Exception ex)
                    {
                        _loggingService.LogError($"Error processing file {inputFile}", ex);
                        result.ErrorCount++;
                        result.ProcessedFiles.Add(new FileResult
                        {
                            InputFile = inputFile,
                            Success = false,
                            ErrorMessage = ex.Message
                        });
                    }
                }

                result.EndTime = DateTime.Now;
                result.Duration = result.EndTime - result.StartTime;
                _loggingService.LogInformation($"Batch processing completed. Success: {result.SuccessCount}, Errors: {result.ErrorCount}");

                return result;
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Fatal error in batch processing", ex);
                result.ErrorMessage = ex.Message;
                return result;
            }
        }

        private async Task<FileResult> ProcessSingleFileAsync(string inputFile, BatchConfiguration config)
        {
            var fileResult = new FileResult
            {
                InputFile = inputFile,
                StartTime = DateTime.Now
            };

            try
            {
                // Load template
                var template = _templateManager.LoadTemplate(config.TemplateName);
                if (template == null)
                {
                    fileResult.ErrorMessage = $"Template {config.TemplateName} not found";
                    return fileResult;
                }

                // Generate output filename
                var fileName = Path.GetFileNameWithoutExtension(inputFile);
                var outputFile = Path.Combine(config.OutputDirectory, $"{fileName}.ctl");

                // Update loader config with current file
                var loaderConfig = template.LoaderConfig.Clone();
                loaderConfig.Infile = inputFile;
                loaderConfig.Badfile = Path.Combine(config.OutputDirectory, $"{fileName}.bad");
                loaderConfig.Discardfile = Path.Combine(config.OutputDirectory, $"{fileName}.dsc");

                // Generate control file
                var controlFileContent = _controlFileGenerator.GenerateControlFile(template.FieldDefinitions, loaderConfig);

                // Save control file
                await File.WriteAllTextAsync(outputFile, controlFileContent);

                fileResult.OutputFile = outputFile;
                fileResult.Success = true;
                fileResult.EndTime = DateTime.Now;
                fileResult.Duration = fileResult.EndTime - fileResult.StartTime;

                _loggingService.LogInformation($"Generated control file: {outputFile}");
            }
            catch (Exception ex)
            {
                fileResult.ErrorMessage = ex.Message;
                fileResult.Success = false;
                _loggingService.LogError($"Error processing {inputFile}", ex);
            }

            return fileResult;
        }

        /// <summary>
        /// Generates a batch script for SQL*Loader execution
        /// </summary>
        public string GenerateBatchScript(List<string> controlFiles, string outputDirectory)
        {
            var script = new StringBuilder();
            script.AppendLine("@echo off");
            script.AppendLine("echo Starting SQL*Loader batch processing");
            script.AppendLine($"echo Output directory: {outputDirectory}");
            script.AppendLine();

            foreach (var controlFile in controlFiles)
            {
                var fileName = Path.GetFileNameWithoutExtension(controlFile);
                script.AppendLine($"echo Processing {fileName}...");
                script.AppendLine($"sqlldr userid=username/password@database control={controlFile} log={Path.Combine(outputDirectory, $"{fileName}.log")}");
                script.AppendLine($"if errorlevel 1 echo Error processing {fileName}");
                script.AppendLine();
            }

            script.AppendLine("echo Batch processing completed");
            script.AppendLine("pause");

            return script.ToString();
        }

        /// <summary>
        /// Batch configuration
        /// </summary>
        public class BatchConfiguration
        {
            public List<string> InputFiles { get; set; } = new();
            public string TemplateName { get; set; } = string.Empty;
            public string OutputDirectory { get; set; } = string.Empty;
            public bool GenerateBatchScript { get; set; } = true;
            public bool OverwriteExisting { get; set; } = false;
        }

        /// <summary>
        /// Batch processing result
        /// </summary>
        public class BatchResult
        {
            public DateTime StartTime { get; set; }
            public DateTime EndTime { get; set; }
            public TimeSpan Duration { get; set; }
            public int TotalFiles { get; set; }
            public int SuccessCount { get; set; }
            public int ErrorCount { get; set; }
            public List<FileResult> ProcessedFiles { get; set; } = new();
            public string ErrorMessage { get; set; } = string.Empty;
        }

        /// <summary>
        /// Individual file processing result
        /// </summary>
        public class FileResult
        {
            public string InputFile { get; set; } = string.Empty;
            public string OutputFile { get; set; } = string.Empty;
            public bool Success { get; set; }
            public string ErrorMessage { get; set; } = string.Empty;
            public DateTime StartTime { get; set; }
            public DateTime EndTime { get; set; }
            public TimeSpan Duration { get; set; }
        }
    }
} 