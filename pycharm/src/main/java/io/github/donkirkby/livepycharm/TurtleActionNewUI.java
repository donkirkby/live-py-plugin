package io.github.donkirkby.livepycharm;

import icons.LivePythonIcons;

import javax.swing.*;

public class TurtleActionNewUI extends TurtleAction {
    @Override
    Icon getDefaultIcon() {
        return LivePythonIcons.TURTLE_DARK;
    }

    @Override
    Icon getPassingIcon() {
        return LivePythonIcons.TURTLE_GREEN_DARK;
    }

    @Override
    Icon getFailingIcon() {
        return LivePythonIcons.TURTLE_RED_DARK;
    }
}
