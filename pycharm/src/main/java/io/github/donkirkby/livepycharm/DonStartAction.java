package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import org.jetbrains.annotations.NotNull;

public class DonStartAction extends AnAction implements DumbAware {
  protected DonStartAction() {
  }
  @Override
  public @NotNull ActionUpdateThread getActionUpdateThread() {
    return ActionUpdateThread.BGT;
  }

  @Override
  public void update(@NotNull AnActionEvent e) {
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
  }
}
