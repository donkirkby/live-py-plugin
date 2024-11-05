plugins {
    id("java")
    id("org.jetbrains.intellij.platform") version "2.0.1"
}

group = "io.github.donkirkby"
version = "4.11.4"

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
        intellijIdeaCommunity("2024.2.2")

        // "Pythonid" for ultimate, "PythonCore" for community. Find compatible
        // ultimate versions at https://plugins.jetbrains.com/plugin/631
        // community versions at https://plugins.jetbrains.com/plugin/7322
        plugin("PythonCore:242.22855.74")

        instrumentationTools()
        pluginVerifier()
    }
}

// Configure IntelliJ Platform Gradle Plugin - read more: https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin-extension.html
intellijPlatform {
    pluginVerification {
        ides {
            // recommended()
            ides(listOf("IC-2023.3", "IC-243.21155.17"))
        }
    }
}

tasks {
    patchPluginXml {
        sinceBuild = "242.22855.74"
        untilBuild = provider { null }
    }
}