package io.github.donkirkby.livepycharm;

import com.intellij.execution.ExecutionManager;
import com.intellij.execution.RunManagerEx;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunnerSettings;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.jetbrains.python.run.PythonRunConfiguration;

public class StartAction extends AnAction {

    @Override
    public void actionPerformed(AnActionEvent e) {
        Editor editor = e.getData(LangDataKeys.EDITOR);

        SplitFileEditor splitFileEditor = SplitFileEditor.getSplitFileEditor(editor);
        if (splitFileEditor == null) {
            return;
        }
        final Project project = e.getProject();
        if (project == null || project.isDisposed()) {
            return;
        }

        RunnerAndConfigurationSettings configuration = RunManagerEx.getInstanceEx(project).getSelectedConfiguration();
        RunConfiguration runConfiguration = (configuration != null) ? configuration.getConfiguration() : null;
        PythonRunConfiguration pythonConfiguration =
                runConfiguration instanceof PythonRunConfiguration
                ? (PythonRunConfiguration) runConfiguration
                : null;

        if (pythonConfiguration != null) {
            splitFileEditor.startAnalysis(pythonConfiguration);
        }
    }
}
