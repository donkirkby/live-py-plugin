import org.jetbrains.intellij.platform.gradle.IntelliJPlatformType

plugins {
    id("java")
    id("org.jetbrains.intellij.platform") version "2.11.0"
}

group = "io.github.donkirkby"
version = "4.13.3"

repositories {
    mavenCentral()

    intellijPlatform {
        defaultRepositories()
    }
}

// Configure Gradle IntelliJ Plugin
// Read more: https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin.html
dependencies {
    intellijPlatform {
        intellijIdeaUltimate("2025.3.2")

        // "Pythonid" for ultimate, "PythonCore" for community. Find compatible
        // ultimate versions at https://plugins.jetbrains.com/plugin/631
        // community versions at https://plugins.jetbrains.com/plugin/7322
        plugin("PythonCore:253.30387.90")

        pluginVerifier()
    }
}

sourceSets {
    main {
        resources {
            srcDirs("../plugin/PySrc")
            exclude("**/*.pyc")
            exclude("*.egg-info")
        }
    }
}

// Configure IntelliJ Platform Gradle Plugin - read more: https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin-extension.html
intellijPlatform {
    pluginVerification {
        ides {
            // recommended()
            // Available versions listed at https://www.jetbrains.com/idea/download/other.html
            create(IntelliJPlatformType.IntellijIdeaCommunity, "2022.2")
            create(IntelliJPlatformType.IntellijIdea, "2025.3.2")
        }
    }
}

tasks {
    patchPluginXml {
        sinceBuild = "222.3345.118"
        untilBuild = provider { null }
    }
}