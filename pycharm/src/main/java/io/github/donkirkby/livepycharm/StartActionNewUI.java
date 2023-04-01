package io.github.donkirkby.livepycharm;

import icons.LivePythonIcons;

import javax.swing.*;

public class StartActionNewUI extends StartAction {

    @Override
    Icon getDefaultIcon() {
        return LivePythonIcons.MEDIA_PLAY_DARK;
    }

    @Override
    Icon getPassingIcon() {
        return LivePythonIcons.MEDIA_PLAY_GREEN_DARK;
    }

    @Override
    Icon getFailingIcon() {
        return LivePythonIcons.MEDIA_PLAY_RED_DARK;
    }
}
